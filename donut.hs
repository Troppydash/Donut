import Data.List (genericLength, groupBy, transpose, sortBy, maximumBy)
import Data.Ord (clamp)
import Debug.Trace (traceShow)
import Data.Function (on)
import System.IO
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent
import System.Process
import qualified Data.Text as Text
import Data.Text.IO (hPutStr)


-- creates a spinning donut ASCII renderer
-- in Haskell!
-- The 3rd entry into the weekly diary project


{- Section 0, helper functions -}
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]
    

extract :: [(Int, a)] -> [a]
extract = map snd

partitionEvery :: Int -> [a] -> [[a]]
partitionEvery n xs = map extract (groupBy condition (enumerate xs))
    where condition = (<) `on` (`rem` n) . fst



everyOther :: [a] -> [a]
everyOther (x:y:xs) = x : everyOther xs
everyOther [x] = [x]
everyOther [] = []


bytesToIntBig :: (Integral a, Num b) => [a] -> b
bytesToIntBig bytes = fromIntegral (foldl accum 0 bytes)
    where accum acc byte = acc * 256 + byte

readTer :: FilePath -> IO (Image RGB)
readTer filename = do
    contents <- BS.readFile filename
    let bytes = BS.unpack contents

    -- the first 8 bytes are the width in little-endian
    let (widthBytes, imageBytes) = splitAt 8 bytes
    let width = bytesToIntBig widthBytes

    -- then read the rgb values
    let pixels = map (\[r, g, b] -> (fromIntegral r, fromIntegral g, fromIntegral b)) (partitionEvery 3 imageBytes)
    return (Image pixels width)


linspace start end step = [start,(start+step)..end]

traceLog x = traceShow (show x) x

{- Section 1, the renderer -}
shadings :: String
shadings = "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. "

data Image a =
    Image {
        pixels :: [a],
        width :: Int
    }
    
type RGB = (Int, Int, Int)
type ASCII = Char

instance Show (Image Char) where
    show (Image pixels width) = unlines $ everyOther $ partitionEvery width pixels
              

brightness :: RGB -> Int
brightness pixel = let (r, g, b) = pixel
                   in round $ fromIntegral (r+g+b) / 3

invert :: Num a => a -> a
invert = (255 -)

asciify :: Int -> Char
asciify brightness =
    shadings !! (size - index - 1)
    where size = length shadings
          index = floor (fromIntegral (brightness * size) / 256)

asciifyImage :: Image RGB -> Image ASCII
asciifyImage image = Image (map asciify brightnessImage) (width image)
    where brightnessImage = map (invert . brightness) (pixels image)

{- Section 3, Donut rendering -}

-- 3D space and donuts

-- TODO! Render Donut
type Vec4 = (Double, Double, Double, Double)
type Vec3 = (Double, Double, Double)
type Vec2 = (Double, Double)

-- position, rotation, R, r
data Donut = Donut Vec3 Vec4 Double Double

rotateDonut :: Donut -> Double -> Donut
rotateDonut (Donut position (x, y, z, _) r' r) t
    = Donut position (x, y, z, t) r' r

-- torus parameteric equation
-- x = R cos(u) + r cos(u)cos(v)
-- y = R sin(u) + r sin(u)cos(v)
-- z = r sin(v)
sampleDonut :: Donut -> [Vec3]
sampleDonut donut = 
    [ pos u v | u <- usamples, v <- vsamples ]
    where (Donut position rotation r' r) = donut
          usamples = linspace 0 6.28 0.05
          vsamples = linspace 0 6.28 0.05
          x u v = r' * cos u + r * cos u * cos v
          y u v = r' * sin u + r * sin u * cos v
          z u v = r * sin v
          pos u v = (x u v, y u v, z u v)

localToWorld :: Matrix Double -> Matrix Double -> [Vec3] -> [Matrix Double]
localToWorld transform rotation = map (matMul transform . matMul rotation . createMatrixFromVec)

worldToScreen :: Matrix Double -> [Matrix Double] -> [Matrix Double]
worldToScreen projection = map (matMul projection)


type Matrix a = [[a]]

matMul :: (Num a) => Matrix a -> Matrix a -> Matrix a
matMul a b = [[ sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

matAdd :: (Num a) => Matrix a -> Matrix a -> Matrix a
matAdd = zipWith (zipWith (+))

vec2Dist (x1, y1) (x2, y2) = sqrt $ (x1-x2) ^ 2 + (y1-y2) ^ 2 

matToPosition mat = 
    let [[_, _, _, x],
         [_, _, _, y],
         [_, _, _, z],
         [_, _, _, _]] = mat
    in (x, y, z)

matToClipping mat = (mat !! 3) !! 3

createMatrixFromVec (x, y, z) =
    [[0, 0, 0, x],
     [0, 0, 0, y],
     [0, 0, 0, z],
     [0, 0, 0, 1]]

createTransformMatrix (x, y, z) (sx, sy, sz) =
    [[sx, 0, 0, x],
     [0, sy, 0, y],
     [0, 0, sz, z],
     [0, 0, 0, 1]]

createOrthographicMatrix top bottom left right far near =
    [[2 / (right - left), 0, 0, -((right+left)/(right-left))],
     [0, 2/(top-bottom), 0, -((top+bottom)/(top-bottom))],
     [0, 0, 2 / (far-near), -((far+near)/(far-near))],
     [0, 0, 0, 1]]

createRotationMatrix (x, y, z) t =
    [[cos t + x^2 * (1-cos t), x*y*(1-cos t) - x * sin t, x * z * (1 - cos t) + y * sin t, 0],
     [y * x * (1 - cos t) + z * sin t, cos t + y^2 * (1 - cos t), y * z * (1 - cos t) - x * sin t, 0],
     [z * x * (1 - cos t) - y * sin t, z * y * (1 - cos t) + x * sin t, cos t + z^2 * (1 - cos t), 0],
     [0, 0, 0, 1]]

camera :: Matrix Double
camera = createOrthographicMatrix 10 (-10) (-10) 10 10 1

computeDonut :: Matrix Double -> Donut -> [Matrix Double]
computeDonut camera donut = 
    screen
    where samples = sampleDonut donut  -- OK!
          (Donut position (x, y, z, t) _ _) = donut  -- OK!
          rotationMatrix = createRotationMatrix (x, y, z) t
          transformMatrix = createTransformMatrix position (1, 1, 1)  -- OK!
          world = localToWorld transformMatrix rotationMatrix samples
          screen = worldToScreen camera world


terminalTransform width height = 
    [[width / 2, 0, 0, width / 2],
     [0, height / 2, 0, height / 2],
     [0, 0, 1, 0],
     [0, 0, 0, 1]]

matToTerminal :: Matrix Double -> (Int, Int, Double)
matToTerminal mat = 
    let [[_, _, _, x],
         [_, _, _, y],
         [_, _, _, d],
         [_, _, _, _]] = mat
    in (round x, round y, d)

drawPixels :: Vec2 -> [Matrix Double] -> Image ASCII
drawPixels size points = 
    Image ([
        asciify $ findPoint x y
        | y <- [0..round (height-1)], x <- [0..round (width-1)]
    ]) (round width)
    where (width, height) = size
          terminalData = map (matToTerminal . matMul (terminalTransform width height)) points
          findPoint x y
            | null closePoints = 0
            | otherwise = clamp (0, 255) brightness
                where closePoints = filter (\(tx, ty, _) -> tx == x && ty == y) terminalData
                      third (_, _, x) = x
                      brightness = let (_, _, depth) = maximumBy (compare `on` third) closePoints in floor (255 * depth)


dt = 0.1
updateLoop donut time = do
    -- render & update
    let points = computeDonut camera donut
    let image =  drawPixels (50, 30) points
    let imageStr = Text.pack $ show image
    clearTerminal

    Data.Text.IO.hPutStr stdout imageStr

    -- sleep dt
    let rotatedDonut = rotateDonut donut time
    updateLoop rotatedDonut (time + dt)


{- Section 4, Tests -}
drawImage filename = do
    image <- readTer filename
    let textImage = asciifyImage image
    print textImage


clearTerminal = putStr ""
-- clearTerminal = system "cls"
sleep seconds = threadDelay $ round $ seconds * 1000000

main = do
    let donut = Donut (0, 0, 6) (0, 1, 0, 0) 4 2 
    updateLoop donut 0
    -- keepPrinting "Hello"
    -- let matrix = [[1, 2],
    --               [3, 4]]
    -- let other = [[1, 0], 
    --              [0, 1]]
    -- print $ matMul matrix other
    -- let donut = Donut (0, 0, 7) (0, 1, 0, 1) 4 2 
    -- clearTerminal
    -- let points = computeDonut camera donut
    -- -- let points = [createMatrixFromVec (5, 2, 5), createMatrixFromVec (1, 4, 2)]
    -- -- print $ take 5 points
    -- let image = drawPixels (100, 50) points
    -- putStrLn $ show image
