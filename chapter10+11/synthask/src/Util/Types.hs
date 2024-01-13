module Util.Types
  ( Sample,
    Signal,
    sampleRate,
    samplesPerPeriod,
    samplesPerSecond,
  )
where

import Composition.Notelength (Seconds)
import Composition.Pitch (Hz)

type Sample = Double

type Signal = [Sample]

sampleRate :: Double
sampleRate = 44100

samplesPerPeriod :: Hz -> Int
samplesPerPeriod hz = round $ sampleRate / hz

samplesPerSecond :: Seconds -> Int
samplesPerSecond duration = round $ duration * sampleRate
