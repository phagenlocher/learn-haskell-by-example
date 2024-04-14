{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.PNM.Conversion
  ( PixelMapping,
    GenericRGB,
    mapImagePixels,
    writeAsPng,
  )
where

import qualified Codec.Picture as P
import Control.Parallel.Strategies as S
import qualified Data.Vector as V
import qualified Data.Vector.Strategies as V
import Data.Word
import Graphics.PNM.Types

type GenericRGB = (Double, Double, Double)

class GenericPixel px where
  toGenericRGB :: px -> GenericRGB
  fromGenericRGB :: GenericRGB -> px

clamp :: Double -> Double
clamp = max 0 . min 1

scaleDown8Bit :: Word8 -> Double
scaleDown8Bit v = clamp $ (fromIntegral v) / 255.0

scaleUp8Bit :: Double -> Word8
scaleUp8Bit v = round $ 255 * (clamp v)

scaleDown16Bit :: Word16 -> Double
scaleDown16Bit v = clamp $ (fromIntegral v) / 65535.0

scaleUp16Bit :: Double -> Word16
scaleUp16Bit v = round $ 65535 * (clamp v)

instance GenericPixel P.Pixel8 where
  toGenericRGB p = (p', p', p')
    where
      p' = scaleDown8Bit p
  fromGenericRGB (r, g, b) = p
    where
      p = scaleUp8Bit $ (r + g + b) / 3

instance GenericPixel P.Pixel16 where
  toGenericRGB p = (p', p', p')
    where
      p' = scaleDown16Bit p / 3
  fromGenericRGB (r, g, b) = p
    where
      p = scaleUp16Bit $ (r + g + b) / 3

instance GenericPixel P.PixelRGB8 where
  toGenericRGB (P.PixelRGB8 r g b) =
    (scaleDown8Bit r, scaleDown8Bit g, scaleDown8Bit b)
  fromGenericRGB (r, g, b) =
    P.PixelRGB8
      (scaleUp8Bit r)
      (scaleUp8Bit g)
      (scaleUp8Bit b)

instance GenericPixel P.PixelRGB16 where
  toGenericRGB (P.PixelRGB16 r g b) =
    (scaleDown16Bit r, scaleDown16Bit g, scaleDown16Bit b)
  fromGenericRGB (r, g, b) =
    P.PixelRGB16
      (scaleUp16Bit r)
      (scaleUp16Bit g)
      (scaleUp16Bit b)

type PixelMapping =
  ( Int -> -- ^ X-pos
    Int -> -- ^ Y-pos
    (Int -> Int -> Maybe GenericRGB) -> -- ^ (x,y) -> Pixel in image
    GenericRGB -> -- ^ Current pixel
    GenericRGB
  )

mapImagePixels ::
  PixelMapping ->
  DynamicPnmImage ->
  DynamicPnmImage
mapImagePixels f (DynamicPnmImage img) =
  case img of
    PnmGray8Bit _ _ pxs ->
      PnmGray8Bit `withPixels` mapping pxs
    PnmGray16Bit _ _ pxs ->
      PnmGray16Bit `withPixels` mapping pxs
    PnmColor8Bit _ _ pxs ->
      PnmColor8Bit `withPixels` mapping pxs
    PnmColor16Bit _ _ pxs ->
      PnmColor16Bit `withPixels` mapping pxs
  where
    w = pnmWidth img
    h = pnmHeight img

    withPixels ::
      (Int -> Int -> V.Vector px -> PnmImage px) ->
      V.Vector px ->
      DynamicPnmImage
    withPixels c pxs = DynamicPnmImage $ c w h pxs

    mapping :: GenericPixel px => V.Vector px -> V.Vector px
    mapping pixels =
      S.withStrategy (V.parVectorChunk ((w * h) `div` 1000)) $
        flip V.imap pixels $ \i px ->
          let x = i `mod` w
              y = i `div` w
              getPixel x' y' =
                toGenericRGB <$> pixels V.!? (y' * w + x')
              result = f x y getPixel $ toGenericRGB px
           in fromGenericRGB result

dynamicPnmToDynamicImage :: DynamicPnmImage -> P.DynamicImage
dynamicPnmToDynamicImage (DynamicPnmImage img) =
  case img of
    PnmGray8Bit {} -> P.ImageY8 $ pnmToImage img
    PnmGray16Bit {} -> P.ImageY16 $ pnmToImage img
    PnmColor8Bit {} -> P.ImageRGB8 $ pnmToImage img
    PnmColor16Bit {} -> P.ImageRGB16 $ pnmToImage img
  where
    pnmToImage :: P.Pixel px => PnmImage px -> P.Image px
    pnmToImage pnmImg =
      P.generateImage build (pnmWidth pnmImg) (pnmHeight pnmImg)
      where
        build x y =
          pnmPixels pnmImg V.! (y * (pnmWidth pnmImg) + x)

writeAsPng ::
  FilePath ->
  DynamicPnmImage ->
  IO (Either String Bool)
writeAsPng path img =
  P.writeDynamicPng path $
    dynamicPnmToDynamicImage img
