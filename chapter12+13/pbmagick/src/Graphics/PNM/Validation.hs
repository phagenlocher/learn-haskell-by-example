{-# LANGUAGE NamedFieldPuns #-}

module Graphics.PNM.Validation (validatePnm) where

import qualified Codec.Picture as P
import qualified Data.Vector as V
import Data.Word (Word16)
import Graphics.PNM.Types

validatePnm :: RawPnm -> Either String DynamicPnmImage
validatePnm img@(RawPnm {header, imageData})
  | not $ imageSizeCorrect img =
    Left $
      "The number of pixels given does not match "
        <> "the expected size inferred from the header"
  | indicatesBitmap magicNum =
    case maxVal header of
      Nothing -> PnmGray8Bit `withTransform` bitmapPixel
      Just _ ->
        Left $
          "Image seems to be a bitmap image "
            <> "but has a maximum value set"
  | indicatesGraymap magicNum =
    case maxVal header of
      Nothing ->
        Left $
          "Image seems to be a graymap image "
            <> "but has no maximum value set"
      Just maxVal ->
        if maxVal <= 255
          then
            PnmGray8Bit
              `withTransform` graymapPixel maxVal
          else
            PnmGray16Bit
              `withTransform` graymapPixel maxVal
  | indicatesPixmap magicNum =
    case maxVal header of
      Nothing ->
        Left $
          "Image seems to be a pixmap image "
            <> "but has no maximum value set"
      Just maxVal ->
        if maxVal <= 255
          then
            PnmColor8Bit
              `withTransform` pixmapPixel maxVal P.PixelRGB8
          else
            PnmColor16Bit
              `withTransform` pixmapPixel maxVal P.PixelRGB16
  | otherwise = Left "Image seems to be of unknown type"
  where
    magicNum = magicNumber header
    width' = fromIntegral $ width header
    height' = fromIntegral $ height header

    withTransform ::
      (Int -> Int -> V.Vector px -> PnmImage px) ->
      (RawPixel -> Either String px) ->
      Either String DynamicPnmImage
    withTransform c f =
      let mConvertedData = mapM f $ pixels imageData
       in fmap mkRes mConvertedData
      where
        mkRes = DynamicPnmImage . c width' height' . V.fromList

imageSizeCorrect :: RawPnm -> Bool
imageSizeCorrect RawPnm {header, imageData} =
  let expectedNumPixels =
        fromIntegral $ width header * height header
      numPixels = length $ pixels imageData
   in numPixels == expectedNumPixels

indicatesBitmap :: MagicNumber -> Bool
indicatesBitmap P1 = True
indicatesBitmap P4 = True
indicatesBitmap _ = False

indicatesGraymap :: MagicNumber -> Bool
indicatesGraymap P2 = True
indicatesGraymap P5 = True
indicatesGraymap _ = False

indicatesPixmap :: MagicNumber -> Bool
indicatesPixmap P3 = True
indicatesPixmap P6 = True
indicatesPixmap _ = False

bitmapPixel :: RawPixel -> Either String P.Pixel8
bitmapPixel (Single 1) = Right 0
bitmapPixel (Single 0) = Right 255
bitmapPixel p =
  Left $
    "Could not convert "
      <> show p
      <> " to a bitmap pixel"

graymapPixel ::
  (Integral px) =>
  Word16 ->
  RawPixel ->
  Either String px
graymapPixel maxVal pixel@(Single g)
  | g > maxVal =
    Left $
      "The pixel "
        <> show pixel
        <> " exceeds the maximum value "
        <> show maxVal
  | otherwise = Right v
  where
    factor = fromIntegral g / fromIntegral maxVal
    v = round $ fromIntegral maxVal * factor
graymapPixel _ p =
  Left $
    "Could not convert "
      <> show p
      <> " to a graymap pixel"

pixmapPixel ::
  (Integral val) =>
  Word16 ->
  (val -> val -> val -> px) ->
  RawPixel ->
  Either String px
pixmapPixel maxVal f pixel@(RGB r g b)
  | r > maxVal || g > maxVal || b > maxVal =
    Left $
      "The pixel "
        <> show pixel
        <> " exceeds the maximum value "
        <> show maxVal
  | otherwise =
    Right $
      f (transform r) (transform g) (transform b)
  where
    factor x = fromIntegral x / fromIntegral maxVal
    transform x = round $ fromIntegral maxVal * factor x
pixmapPixel _ _ p =
  Left $
    "Could not convert "
      <> show p
      <> " to a pixmap pixel"
