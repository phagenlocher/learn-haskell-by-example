module Data.Vector.Strategies (parVectorChunk) where

import Control.Parallel.Strategies
import qualified Data.Vector as V

parVectorChunk :: Int -> Strategy (V.Vector a)
parVectorChunk size =
  fmap V.fromList . parListChunk size rseq . V.toList
