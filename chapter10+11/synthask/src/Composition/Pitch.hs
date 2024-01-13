{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Composition.Pitch
  ( Hz,
    Pitchable (..),
    Pitch (..),
    Semitone,
    Chromatic,
    ChromaticName,
    chromaticToSemitone,
    a,
    as,
    b,
    c,
    cs,
    d,
    ds,
    e,
    f,
    fs,
    g,
    gs,
    midiNoteToSemitone,
    semitoneToMidiNote,
  )
where

import Codec.Midi (Key)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (arbitrary), elements)

type Hz = Double

class Pitchable a where
  toFrequency :: a -> Hz

data Pitch = forall a. (Pitchable a, Show a) => Pitch a

deriving instance Show Pitch

instance Pitchable Pitch where
  toFrequency (Pitch p) = toFrequency p

instance Pitchable Hz where
  toFrequency = id

type Semitone = Integer

instance Pitchable Semitone where
  toFrequency semitone =
    440 * (2.0 ** (fromInteger semitone / 12))

data ChromaticName = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs
  deriving (Read, Show, Eq)

data Chromatic
  = Chromatic ChromaticName Natural
  deriving (Eq)

instance Show Chromatic where
  show (Chromatic name oct) = show name ++ show oct

instance Arbitrary ChromaticName where
  arbitrary = elements [A, As, B, C, Cs, D, Ds, E, F, Fs, G, Gs]

instance Arbitrary Chromatic where
  arbitrary = do
    name <- arbitrary
    octave <- elements [1 .. 10]
    return $ Chromatic name octave

chromaticToSemitone :: Chromatic -> Semitone
chromaticToSemitone (Chromatic name oct) =
  (12 * (fromIntegral oct - 4)) + noteOffset name
  where
    noteOffset C = -9
    noteOffset Cs = -8
    noteOffset D = -7
    noteOffset Ds = -6
    noteOffset E = -5
    noteOffset F = -4
    noteOffset Fs = -3
    noteOffset G = -2
    noteOffset Gs = -1
    noteOffset A = 0
    noteOffset As = 1
    noteOffset B = 2

instance Pitchable Chromatic where
  toFrequency = toFrequency . chromaticToSemitone

a :: Natural -> Chromatic
a = Chromatic A

as :: Natural -> Chromatic
as = Chromatic As

b :: Natural -> Chromatic
b = Chromatic B

c :: Natural -> Chromatic
c = Chromatic C

cs :: Natural -> Chromatic
cs = Chromatic Cs

d :: Natural -> Chromatic
d = Chromatic D

ds :: Natural -> Chromatic
ds = Chromatic Ds

e :: Natural -> Chromatic
e = Chromatic E

f :: Natural -> Chromatic
f = Chromatic F

fs :: Natural -> Chromatic
fs = Chromatic Fs

g :: Natural -> Chromatic
g = Chromatic G

gs :: Natural -> Chromatic
gs = Chromatic Gs

{-
For those who want to play around with MIDI ;)
-}

midiNoteToSemitone :: Codec.Midi.Key -> Semitone
midiNoteToSemitone k = fromIntegral $ k - 69

semitoneToMidiNote :: Semitone -> Codec.Midi.Key
semitoneToMidiNote k = fromIntegral $ k + 69

instance Pitchable Codec.Midi.Key where
  toFrequency = toFrequency . midiNoteToSemitone
