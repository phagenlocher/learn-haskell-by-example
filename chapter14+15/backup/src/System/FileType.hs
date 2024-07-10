module System.FileType
  ( FileType (..),
    allFileTypes,
    determineFileType,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import System.IO (IOMode (ReadMode), hClose, openFile)

data FileType
  = Other
  | Image
  | Compressed
  deriving (Show, Read, Eq, Enum, Bounded)

allFileTypes :: [FileType]
allFileTypes = enumFromTo minBound maxBound

determineFileType :: FilePath -> IO FileType
determineFileType path =
  bracket (openFile path ReadMode) hClose $ \hdl -> do
    signature <- BS.hGet hdl 8
    return $ case BS.unpack signature of
      -- Bitmap
      0x42 : 0x4d : _ -> Image
      -- PNG
      0x89 : 0x50 : 0x4e : 0x47 : 0x0d : 0x0a : 0x1a : 0x0a : _ -> Image
      -- JPEG
      0xff : 0xd8 : 0xff : _ -> Image
      -- Bzip2
      0x42 : 0x5a : 0x68 : _ -> Compressed
      -- Gzip
      0x1f : 0x8b : _ -> Compressed
      _ -> Other
