module Sound.Sound
  ( mix,
    Performer (..),
  )
where

import Composition.Notelength
import Composition.Performance
import Data.List
import Sound.Sampler
import Sound.Synth
import Util.Types

mix :: [Signal] -> Signal
mix signals = (/ n) <$> foldl' addSignals [] signals
  where
    n :: Double
    n = fromIntegral $ length signals

    addSignals :: Signal -> Signal -> Signal
    addSignals xs [] = xs
    addSignals [] ys = ys
    addSignals (x : xs) (y : ys) = (x + y) : addSignals xs ys

class Performer p where
  play :: p -> Performance -> Signal

instance Performer Oscillator where
  play (Osc oscf) perf = mix $ fmap (playEvents 0) eventGroups
    where
      eventGroups :: [[Event]]
      eventGroups = foldr insertGroup [] perf
        where
          insertGroup x [] = [[x]]
          insertGroup x (es : ess)
            | or [x `overlaps` e | e <- es] = es : insertGroup x ess
            | otherwise = (x : es) : ess

      playEvents :: Seconds -> [Event] -> Signal
      playEvents _ [] = []
      playEvents curTime (event : xs)
        | curTime < ts =
            concat
              [ silence (ts - curTime),
                oscf event,
                playEvents te xs
              ]
        | curTime == ts = oscf event ++ playEvents te xs
        | otherwise = error "Event occurs in the past!"
        where
          ts = start event
          te = end event

instance Performer Sampler where
  play (Sampler sampleMap) perf = mix $ fmap (playEvents 0) eventGroups
    where
      playSample :: Event -> Signal
      playSample (Silence _ dur) = silence dur
      playSample (Tone freq _ dur) =
        let numSamples = samplesPerSecond dur
            signal = getSample sampleMap freq dur
         in signal ++ replicate (numSamples - length signal) 0

      eventGroups :: [[Event]]
      eventGroups = foldr insertGroup [] perf
        where
          insertGroup x [] = [[x]]
          insertGroup x (es : ess)
            | or [x `overlaps` e | e <- es] = es : insertGroup x ess
            | otherwise = (x : es) : ess

      playEvents :: Seconds -> [Event] -> Signal
      playEvents _ [] = []
      playEvents curTime (event : xs)
        | curTime < ts =
            concat
              [ silence (ts - curTime),
                playSample event,
                playEvents te xs
              ]
        | curTime == ts = playSample event ++ playEvents te xs
        | otherwise = error "Event occurs in the past!"
        where
          ts = start event
          te = end event
