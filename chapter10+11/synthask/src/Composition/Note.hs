module Composition.Note
  ( NoteStructure (..),
    (.|),
    p,
    (<~>),
    (<~|),
    (|~>),
    (<:>),
    (<:|),
    (|:>),
    (<~!>),
    (<:!>),
  )
where

import Composition.Notelength
  ( Notelength,
  )
import Composition.Pitch (Pitch (..), Pitchable)
import Numeric.Natural (Natural)

data NoteStructure
  = Note Notelength Pitch
  | Pause Notelength
  | Sequence [NoteStructure]
  | Group [NoteStructure]
  deriving (Show)

(.|) :: (Pitchable a, Show a) => a -> Notelength -> NoteStructure
(.|) pitch l = Note l (Pitch pitch)

infixr 4 .|

p :: Notelength -> NoteStructure
p = Pause

(<~>) :: NoteStructure -> NoteStructure -> NoteStructure
(<~>) (Sequence xs) (Sequence ys) = Sequence $ xs ++ ys
(<~>) (Sequence xs) x = Sequence $ xs ++ [x]
(<~>) x (Sequence xs) = Sequence $ x : xs
(<~>) a b = Sequence [a, b]

infixr 3 <~>

(<~|) :: NoteStructure -> Natural -> NoteStructure
(<~|) (Sequence xs) n = Sequence $ concat [xs | _ <- [1 .. n]]
(<~|) struct n = Sequence $ [struct | _ <- [1 .. n]]

infixr 1 <~|

(|~>) :: Natural -> NoteStructure -> NoteStructure
(|~>) = flip (<~|)

infixr 1 |~>

(<:>) :: NoteStructure -> NoteStructure -> NoteStructure
(<:>) (Group xs) (Group ys) = Group $ xs ++ ys
(<:>) (Group xs) x = Group $ xs ++ [x]
(<:>) x (Group xs) = Group $ x : xs
(<:>) a b = Group [a, b]

infixr 2 <:>

(<:|) :: NoteStructure -> Natural -> NoteStructure
(<:|) (Group xs) n = Group $ concat [xs | _ <- [1 .. n]]
(<:|) struct n = Group $ [struct | _ <- [1 .. n]]

infixr 1 <:|

(|:>) :: Natural -> NoteStructure -> NoteStructure
(|:>) = flip (<:|)

infixr 1 |:>

{-
We made the choice that the (<~>) takes precedence over (<:>). However, we could
have avoided this issue by creating synonyms for both operators with their
precedences switched. The composer then wouldn’t need to use parentheses, but could
switch to the other operator! Implement these synonyms.
-}

(<~!>) :: NoteStructure -> NoteStructure -> NoteStructure
(<~!>) = (<~>)

infixr 2 <~!>

(<:!>) :: NoteStructure -> NoteStructure -> NoteStructure
(<:!>) = (<:>)

infixr 3 <:!>
