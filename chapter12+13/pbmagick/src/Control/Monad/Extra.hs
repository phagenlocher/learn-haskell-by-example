module Control.Monad.Extra where

monadd :: (Num a, Monad m) => m a -> m a -> m a
monadd xm ym =
  xm >>= (\x -> ym >>= (\y -> return $ x + y))

monadd' :: (Num a, Monad m) => m a -> m a -> m a
monadd' xm ym = do
  x <- xm
  y <- ym
  return $ x + y

mhead :: (MonadFail m) => m [a] -> m a
mhead m = do
  (x : _) <- m
  return x
