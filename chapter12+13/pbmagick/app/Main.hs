module Main (main) where

import qualified Codec.Picture as P
import Data.Maybe (catMaybes, fromMaybe)
import Graphics.PNM
import System.Environment (getArgs)

boxBlur :: Int -> PixelMapping
boxBlur boxSize x y px _ =
  let nbrs =
        catMaybes
          [ px (x + dx) (y + dy)
            | dx <- [(-boxSize) .. boxSize],
              dy <- [(-boxSize) .. boxSize]
          ]
      (rx, gx, bx) = unzip3 nbrs
      numPixs = fromIntegral $ length nbrs
   in ( sum rx / numPixs,
        sum gx / numPixs,
        sum bx / numPixs
      )

grayScale :: PixelMapping
grayScale _ _ _ (r, g, b) = (v, v, v)
  where
    v = (r + g + b) / 3

bw :: PixelMapping
bw _ _ _ (r, g, b) = (v, v, v)
  where
    v = fromIntegral . round $ (r + g + b) / 3

pixelate :: Int -> PixelMapping
pixelate pixelSize x y getPx curPx =
  let xDiff = x `mod` pixelSize
      yDiff = y `mod` pixelSize
   in fromMaybe curPx $ getPx (x - xDiff) (y - yDiff)

idMapping :: PixelMapping
idMapping _ _ _ px = px

main :: IO ()
main = do
  [filePath] <- getArgs
  Right rawImg <- readPnmFile filePath
  let Right pnmImg = validatePnm rawImg
      pnmImg' = mapImagePixels (boxBlur 8) pnmImg
  _ <- writeAsPng "output.png" pnmImg'
  return ()
