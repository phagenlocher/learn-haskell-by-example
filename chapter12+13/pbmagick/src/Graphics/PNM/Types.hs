{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Graphics.PNM.Types where

import qualified Codec.Picture as P
import qualified Data.Vector as V
import Data.Word

data MagicNumber = P1 | P2 | P3 | P4 | P5 | P6
  deriving (Eq, Show)

data Header = Header
  { magicNumber :: MagicNumber,
    width :: Integer,
    height :: Integer,
    maxVal :: Maybe Word16
  }
  deriving (Eq, Show)

data RawPixel
  = Single Word16
  | RGB Word16 Word16 Word16
  deriving (Eq, Show)

newtype RawData = RawData
  { pixels :: [RawPixel]
  }
  deriving (Eq, Show)

data RawPnm = RawPnm
  { header :: Header,
    imageData :: RawData
  }
  deriving (Eq, Show)

data PnmImage px where
  PnmGray8Bit ::
    Int ->
    Int ->
    V.Vector P.Pixel8 ->
    PnmImage P.Pixel8
  PnmGray16Bit ::
    Int ->
    Int ->
    V.Vector P.Pixel16 ->
    PnmImage P.Pixel16
  PnmColor8Bit ::
    Int ->
    Int ->
    V.Vector P.PixelRGB8 ->
    PnmImage P.PixelRGB8
  PnmColor16Bit ::
    Int ->
    Int ->
    V.Vector P.PixelRGB16 ->
    PnmImage P.PixelRGB16

pnmWidth :: PnmImage px -> Int
pnmWidth (PnmGray8Bit w _ _) = w
pnmWidth (PnmGray16Bit w _ _) = w
pnmWidth (PnmColor8Bit w _ _) = w
pnmWidth (PnmColor16Bit w _ _) = w

pnmHeight :: PnmImage px -> Int
pnmHeight (PnmGray8Bit _ h _) = h
pnmHeight (PnmGray16Bit _ h _) = h
pnmHeight (PnmColor8Bit _ h _) = h
pnmHeight (PnmColor16Bit _ h _) = h

pnmPixels :: PnmImage px -> V.Vector px
pnmPixels (PnmGray8Bit _ _ pixels) = pixels
pnmPixels (PnmGray16Bit _ _ pixels) = pixels
pnmPixels (PnmColor8Bit _ _ pixels) = pixels
pnmPixels (PnmColor16Bit _ _ pixels) = pixels

deriving instance (Show px) => Show (PnmImage px)

deriving instance (Eq px) => Eq (PnmImage px)

data DynamicPnmImage = forall px. DynamicPnmImage (PnmImage px)
