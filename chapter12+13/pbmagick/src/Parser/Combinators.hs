{-# LANGUAGE OverloadedStrings #-}

module Parser.Combinators
  ( (<|>),
    optional,
    empty,
  )
where

import Control.Applicative (Alternative, Applicative, empty, optional, (<|>))
import Control.Monad
import Data.Foldable (asum)

between :: Applicative m => m open -> m close -> m a -> m a
between o c p = o *> p <* c

choice :: (Foldable f, Alternative m) => f (m a) -> m a
choice = asum

count :: Monad m => Int -> m a -> m [a]
count = replicateM

count' :: MonadPlus m => Int -> Int -> m a -> m [a]
count' m n p
  | m > n || n <= 0 = return []
  | otherwise = count m p >>= go (n - m)
  where
    go num = undefined

eitherP :: Alternative m => m a -> m b -> m (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
