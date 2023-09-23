module Data.Candy where

data Candy
  = Lemon
  | Apple
  | Coffee
  | Caramel
  deriving (Eq, Show)

type CandyTrail = [Candy]

walkOnTrail :: (a -> Candy -> a) -> a -> CandyTrail -> a
walkOnTrail _ hand [] = hand
walkOnTrail f hand (x : xs) = walkOnTrail f (f hand x) xs

isFruity :: Candy -> Bool
isFruity c = c == Lemon || c == Apple

collectFruits :: CandyTrail -> [Candy]
collectFruits =
  walkOnTrail
    ( \hand c ->
        if isFruity c
          then hand ++ [c]
          else hand
    )
    []

collectLastFive :: CandyTrail -> [Candy]
collectLastFive =
  walkOnTrail
    ( \hand c ->
        if length hand == 5
          then tail hand ++ [c]
          else hand ++ [c]
    )
    []

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft _ z [] = z
foldLeft f z (x : xs) = foldLeft f (f z x) xs

foldRight :: (b -> a -> a) -> a -> [b] -> a
foldRight _ z [] = z
foldRight f z (x : xs) = f x $ foldRight f z xs
