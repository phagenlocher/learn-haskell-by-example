module Composition.Performance
  ( Event (..),
    Performance,
    isTone,
    isSilence,
    end,
    overlaps,
    toPerformance,
  )
where

import Composition.Note (NoteStructure (..))
import Composition.Notelength
  ( Seconds,
    TempoInfo,
    timePerNotelength,
  )
import Composition.Pitch (Hz, Pitchable (..))
import Data.List (foldl', partition, sortBy)
import Test.QuickCheck
import Prelude hiding (length)

data Event
  = Tone {freq :: Hz, start :: Seconds, duration :: Seconds}
  | Silence {start :: Seconds, duration :: Seconds}
  deriving (Eq, Show)

instance Arbitrary Event where
  arbitrary = do
    freq <- choose (20, 20000)
    start <- choose (0, 100)
    duration <- choose (0, 100)
    hasTone <- arbitrary
    return $
      if hasTone
        then Tone freq start duration
        else Silence start duration

type Performance = [Event]

isTone :: Event -> Bool
isTone Tone {} = True
isTone _ = False

isSilence :: Event -> Bool
isSilence Silence {} = True
isSilence _ = False

end :: Event -> Seconds
end e = start e + duration e

overlaps :: Event -> Event -> Bool
overlaps e1 e2 =
  start e1 `between` (start e2, end e2)
    || start e2 `between` (start e1, end e1)
  where
    between x (a, b) = x >= a && x <= b

{-
While our toPerformance function works fine there is one possibility for
improvement. Pauses always result in silence. Our NoteStructure type makes it
possible to create a sequence of silences. Of course, these silences can be consolidated
into on large silence. Write a function Performance -> Performance that does
exactly that and add it to the toPerformance function. Think about how we can test
this function. Can you come up with a QuickCheck property for this function?
-}

consolidateSilence :: Performance -> Performance
consolidateSilence [] = []
consolidateSilence (e : es) = case e of
  Tone {} -> e : consolidateSilence es
  Silence s d -> Silence {start = nstart, duration = nduration} : consolidateSilence rest
    where
      (slc, rest) = partition (\x -> isSilence x && e `overlaps` x) es
      (nstart, nduration) =
        foldr (\ev (st, du) -> (min st (start ev), max du (end ev))) (s, d) slc

structureToPerformance ::
  TempoInfo ->
  Seconds ->
  NoteStructure ->
  (Seconds, Performance)
structureToPerformance tempoInfo start structure =
  case structure of
    (Note length pitch) ->
      let freq = toFrequency pitch
          duration = timePerNotelength tempoInfo length
       in (start + duration, [Tone {freq, start, duration}])
    (Pause length) ->
      let duration = timePerNotelength tempoInfo length
       in (start + duration, [Silence {start, duration}])
    (Group structs) -> foldl' f (start, []) structs
      where
        f (durAcc, perf) struct =
          let (dur, tones) = structureToPerformance tempoInfo start struct
           in (max dur durAcc, perf ++ tones)
    (Sequence structs) -> foldl' f (start, []) structs
      where
        f (durAcc, perf) struct =
          let (newdur, tones) = structureToPerformance tempoInfo durAcc struct
           in (newdur, perf ++ tones)

toPerformance :: TempoInfo -> NoteStructure -> Performance
toPerformance tempoInfo =
  consolidateSilence
    . sortBy (\x y -> compare (start x) (start y))
    . snd
    . structureToPerformance tempoInfo 0
