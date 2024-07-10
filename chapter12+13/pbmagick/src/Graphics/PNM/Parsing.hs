module Graphics.PNM.Parsing
  ( parsePnm,
    readPnmFile,
  )
where

import Control.Monad
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C8
import Data.Bits
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Word
import Graphics.PNM.Types
import Prelude hiding (takeWhile)

magicNumberP1P :: Parser MagicNumber
magicNumberP1P = P1 <$ string "P1"

magicNumberP2P :: Parser MagicNumber
magicNumberP2P = P2 <$ string "P2"

magicNumberP3P :: Parser MagicNumber
magicNumberP3P = P3 <$ string "P3"

magicNumberP4P :: Parser MagicNumber
magicNumberP4P = P4 <$ string "P4"

magicNumberP5P :: Parser MagicNumber
magicNumberP5P = P5 <$ string "P5"

magicNumberP6P :: Parser MagicNumber
magicNumberP6P = P6 <$ string "P6"

whitespace :: Parser ()
whitespace =
  void $
    C8.satisfy $
      \c ->
        c == ' '
          || c == '\n'
          || c == '\t'
          || c == '\r'
          || c == '\v'
          || c == '\f'

whitespaces :: Parser ()
whitespaces = skipMany1 whitespace

comment :: Parser BS.ByteString
comment = C8.char '%' >> C8.takeWhile (/= '\n')

skipWhitespace :: Parser ()
skipWhitespace =
  option () $
    choice
      [ comment >> skipWhitespace,
        whitespaces >> skipWhitespace
      ]

headerP :: Parser Header
headerP = do
  skipWhitespace
  magicNumber <-
    choice
      [ magicNumberP1P,
        magicNumberP2P,
        magicNumberP3P,
        magicNumberP4P,
        magicNumberP5P,
        magicNumberP6P
      ]
  skipWhitespace
  width <- C8.decimal
  skipWhitespace
  height <- C8.decimal
  maxVal <-
    if magicNumber == P1 || magicNumber == P4
      then return Nothing
      else Just <$> (skipWhitespace *> C8.decimal)
  whitespace
  return Header {..}

p1PixelP :: Parser RawPixel
p1PixelP =
  Single
    <$> choice
      [ 1 <$ C8.char '1',
        0 <$ C8.char '0'
      ]

p2PixelP :: Parser RawPixel
p2PixelP = Single <$> C8.decimal

p3PixelP :: Parser RawPixel
p3PixelP = do
  r <- C8.decimal
  whitespaces
  g <- C8.decimal
  whitespaces
  b <- C8.decimal
  return $ RGB r g b

p4PixelP ::
  Parser
    ( RawPixel,
      RawPixel,
      RawPixel,
      RawPixel,
      RawPixel,
      RawPixel,
      RawPixel,
      RawPixel
    )
p4PixelP = do
  w <- anyWord8
  let parseBitAtIndex n =
        Single $ if testBit w n then 1 else 0
  return
    ( parseBitAtIndex 7,
      parseBitAtIndex 6,
      parseBitAtIndex 5,
      parseBitAtIndex 4,
      parseBitAtIndex 3,
      parseBitAtIndex 2,
      parseBitAtIndex 1,
      parseBitAtIndex 0
    )

p4DataP :: Int -> Int -> Parser [RawPixel]
p4DataP width height = do
  rows <- count height (reverse <$> readRow width [])
  return $ concat rows
  where
    readRow n pxs
      | n <= 0 = return pxs
      | otherwise = do
          (b7, b6, b5, b4, b3, b2, b1, b0) <- p4PixelP
          case n of
            1 -> return $ b7 : pxs
            2 -> return $ b6 : b7 : pxs
            3 -> return $ b5 : b6 : b7 : pxs
            4 -> return $ b4 : b5 : b6 : b7 : pxs
            5 -> return $ b3 : b4 : b5 : b6 : b7 : pxs
            6 -> return $ b2 : b3 : b4 : b5 : b6 : b7 : pxs
            7 -> return $ b1 : b2 : b3 : b4 : b5 : b6 : b7 : pxs
            8 -> return $ b0 : b1 : b2 : b3 : b4 : b5 : b6 : b7 : pxs
            _ ->
              readRow
                (n - 8)
                (b0 : b1 : b2 : b3 : b4 : b5 : b6 : b7 : pxs)

anyWord16 :: Parser Word16
anyWord16 = do
  h <- fromIntegral <$> anyWord8
  l <- fromIntegral <$> anyWord8
  return $ shift h 8 .|. l

p5PixelWord8P :: Parser RawPixel
p5PixelWord8P = Single <$> (fromIntegral <$> anyWord8)

p5PixelWord16P :: Parser RawPixel
p5PixelWord16P = Single <$> anyWord16

p6PixelWord8P :: Parser RawPixel
p6PixelWord8P = RGB <$> p <*> p <*> p
  where
    p = fromIntegral <$> anyWord8

p6PixelWord16P :: Parser RawPixel
p6PixelWord16P = RGB <$> anyWord16 <*> anyWord16 <*> anyWord16

dataP :: Header -> Parser RawData
dataP (Header {..}) = do
  pixels <- case (magicNumber, maxVal) of
    (P1, _) -> count' $ whitespaces' *> p1PixelP
    (P2, _) -> count' $ whitespaces' *> p2PixelP
    (P3, _) -> count' $ whitespaces' *> p3PixelP
    (P4, _) -> p4DataP width' height'
    (P5, Just mv) ->
      count' $
        if mv <= 255
          then p5PixelWord8P
          else p5PixelWord16P
    (P6, Just mv) ->
      count' $
        if mv <= 255
          then p6PixelWord8P
          else p6PixelWord16P
    _ ->
      error $
        "Internal error: "
          ++ show magicNumber
          ++ " without maximum value in header!"
  return $ RawData pixels
  where
    whitespaces' = option () whitespaces
    count' = count (width' * height')
    width' = fromInteger width
    height' = fromInteger height

pnmP :: Parser RawPnm
pnmP = do
  header <- headerP
  imageData <- dataP header
  option () whitespaces
  return RawPnm {..}

parsePnm :: BS.ByteString -> Either String RawPnm
parsePnm = parseOnly (pnmP <* endOfInput)

readPnmFile :: FilePath -> IO (Either String RawPnm)
readPnmFile path = parsePnm <$> BS.readFile path
