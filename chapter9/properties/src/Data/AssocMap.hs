module Data.AssocMap
  ( AssocMap,
    genAssocMap,
    shrinkAssocMap,
    empty,
    member,
    alter,
    delete,
    insert,
    lookup,
    findWithDefault,
    elems,
    prop_lookup,
    prop_insertTwice,
    prop_delete,
    prop_empty,
    prop_alter,
  )
where

import qualified Data.List as L
import Test.QuickCheck
import Prelude hiding (lookup)

newtype AssocMap k v = AssocMap [(k, v)]
  deriving (Show)

genAssocMap :: (Eq k, Arbitrary k, Arbitrary v) => Gen (AssocMap k v)
genAssocMap = do
  keys <- L.nub <$> arbitrary
  vals <- vectorOf (L.length keys) arbitrary
  return $ AssocMap (L.zip keys vals)

shrinkAssocMap ::
  (Eq k, Arbitrary k, Arbitrary v) =>
  AssocMap k v ->
  [AssocMap k v]
shrinkAssocMap (AssocMap xs) =
  L.map
    (AssocMap . L.nubBy (\(k1, _) (k2, _) -> k1 == k2))
    (shrink xs)

instance
  (Eq k, Arbitrary k, Arbitrary v) =>
  Arbitrary (AssocMap k v)
  where
  arbitrary = genAssocMap
  shrink = shrinkAssocMap

empty :: AssocMap k v
empty = AssocMap []

member :: Eq k => k -> AssocMap k v -> Bool
member key (AssocMap xs) = member' key xs
  where
    member' :: Eq k => k -> [(k, v)] -> Bool
    member' _ [] = False
    member' x ((x', _) : xs)
      | x' == x = True
      | otherwise = member' x xs

alter :: Eq k => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter f key (AssocMap xs) = AssocMap (alter' f key xs)
  where
    alter' :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
    alter' f key [] =
      case f Nothing of
        Nothing -> []
        Just value -> [(key, value)]
    alter' f key ((key', value') : xs)
      | key == key' =
        case f (Just value') of
          Nothing -> xs
          Just value -> (key, value) : xs
      | otherwise =
        (key', value') : alter' f key xs

delete :: Eq k => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: Eq k => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

lookup :: Eq k => k -> AssocMap k v -> Maybe v
lookup key (AssocMap xs) = lookup' key xs
  where
    lookup' key [] = Nothing
    lookup' key ((key', value) : xs)
      | key == key' = Just value
      | otherwise = lookup' key xs

findWithDefault :: (Eq k) => v -> k -> AssocMap k v -> v
findWithDefault defaultValue key map =
  case lookup key map of
    Nothing -> defaultValue
    Just value -> value

elems :: AssocMap k v -> [v]
elems (AssocMap xs) = map snd xs

{-
Since we have implemented an instance of the Arbitrary typeclass for AssocMap it
is not your turn, to write properties for the type that can be tested.
It’s important to check the basic function of this type as a map so we need to check:
  * A value can be looked up after it has been inserted
  * Inserting under the same key multiple times overwrites the already present value
  * Deleting a key works as intended
  * empty is indeed empty
  * Altering the map does not invalidate the invariant
-}

prop_lookup am k v = label' $ lookup k (insert k v am) == Just v
  where
    label' =
      label
        ( if lookup k am == Nothing
            then "Key not present before insertion"
            else "Key present before insertion"
        )
    types = (k :: Int, v :: Int)

prop_insertTwice am k v1 v2 =
  v1 /= v2 -- The test is only valid if v1 and v2 are not the same
    ==> label'
    $ lookup k (insert k v2 (insert k v1 am)) == Just v2
  where
    label' =
      label
        ( if lookup k am == Nothing
            then "Key not present before insertion"
            else "Key present before insertion"
        )
    types = (k :: Int, v1 :: Int, v2 :: Int)

prop_delete am k =
  label' $ lookup k (delete k am) == Nothing
  where
    label' =
      label
        ( if lookup k am == Nothing
            then "Key not present before deletion"
            else "Key present before deletion"
        )
    -- We use `Bool` as a key so it is more propable that the key occurs
    types = am :: AssocMap Bool Int

prop_empty k = withMaxSuccess 10000 $ not (member (k :: Int) empty)

prop_alter am k mV =
  withMaxSuccess 1000 $ label' invariant_prop
  where
    invariant_prop =
      let AssocMap xs = alter (const mV) k am
       in L.length xs == L.length (L.nubBy (\(k1, _) (k2, _) -> k1 == k2) xs)

    label' =
      let value =
            case mV of
              Just _ -> "Just"
              Nothing -> "Nothing"
       in label
            ( if lookup k am == Nothing
                then "Key not present before altering with " ++ value
                else "Key present before altering with " ++ value
            )

    types = (k :: Int, mV :: Maybe Int)
