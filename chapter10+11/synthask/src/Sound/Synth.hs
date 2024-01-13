module Sound.Synth where

import Composition.Notelength
import Composition.Performance
import Composition.Pitch
import Util.Types

type Wave = Double -> Sample

sin :: Wave
sin t = Prelude.sin $ 2 * pi * t

sqw :: Wave
sqw t
  | t <= 0.5 = -1
  | otherwise = 1

saw :: Wave
saw t
  | t < 0 = -1
  | t > 1 = 1
  | otherwise = (2 * t) - 1

tri :: Wave
tri t
  | t < 0 = -1
  | t > 1 = -1
  | t < 0.5 = 4 * t - 1
  | otherwise = -4 * t + 3

{-
For our waveforms we have implemented a square wave. The square wave is special
in that it is actually a so called pulse wave with a 50% duty cycle, meaning that the
amount of time the signal is "low" is equal to the amount of time the signal is "high".
Pulse waves can be defined for any duty cycle and when they are used for music, the
sound characteristics do actually change with the duty cycle! Implement a generic
pulse wave form with adjustable duty cycle.
-}

pls :: Double -> Wave
pls p t
  | t <= p = -1
  | otherwise = 1

silence :: Seconds -> Signal
silence t = replicate (samplesPerSecond t) 0

tone :: Wave -> Hz -> Seconds -> Signal
tone wave freq t = map wave periodValues
  where
    numSamples = samplesPerPeriod freq
    periodValues =
      map
        (\x -> fromIntegral (x `mod` numSamples) / fromIntegral numSamples)
        [0 .. samplesPerSecond t]

{-
Just like `repeat` creates an infinite list with a single value, the `cycle` function takes
a list as an argument and cycles through the values of that list infinitely. In our `tone`
function we manually calculated the cycling waveform using mod. Now, use `cycle`
and `take` to re-implement the `tone` function, by computing the waveform once and
then cycling through it.
-}

tone' :: Wave -> Hz -> Seconds -> Signal
tone' wave freq t = take (samplesPerSecond t) $ cycle waveform
  where
    numSamples = fromIntegral $ samplesPerPeriod freq
    waveform = map (wave . (/ numSamples)) [0.0 .. numSamples]

newtype Oscillator = Osc {playEvent :: Event -> Signal}

osc :: Wave -> ADSR -> Oscillator
osc wave adsrParams = Osc oscf
  where
    oscf (Silence _ t) = silence t
    oscf (Tone f _ t) = adsr adsrParams $ tone' wave f t

data ADSR = ADSR
  { attack :: Seconds,
    decay :: Seconds,
    sustain :: Double,
    release :: Seconds
  }
  deriving (Show)

adsr :: ADSR -> Signal -> Signal
adsr (ADSR a d s r) signal =
  zipWith3
    (\adsCurve rCurve sample -> adsCurve * rCurve * sample)
    (att ++ dec ++ sus)
    rel
    signal
  where
    attackSamples = fromIntegral $ samplesPerSecond a
    decaySamples = fromIntegral $ samplesPerSecond d
    releaseSamples = fromIntegral $ samplesPerSecond r

    att = map (/ attackSamples) [0.0 .. attackSamples]
    dec =
      reverse $
        map
          (\x -> ((x / decaySamples) * (1 - s)) + s)
          [0.0 .. decaySamples - 1]
    sus = repeat s
    -- release modifies sustain in zipWith3
    rel =
      reverse $
        take
          (length signal)
          (map (/ releaseSamples) [0.0 .. releaseSamples] ++ repeat 1.0)

piano :: Oscillator
piano = osc saw $ ADSR 0.01 0.6 0.3 0.2

ocarina :: Oscillator
ocarina = osc Sound.Synth.sin $ ADSR 0.01 0.3 0.7 0.01

violin :: Oscillator
violin = osc saw $ ADSR 2 2 0.6 0.2

pluck :: Oscillator
pluck = osc sqw $ ADSR 0.01 0.05 0.0 0.01

bass :: Oscillator
bass = osc tri $ ADSR 0.001 0.2 0.9 0.1

{-
We have created a basic set of functionalities for sound generation.
Synthesizers in the real world offer a bit more flexibility in terms
of sound shaping, implementing filters and other types of envelopes.
Extend our oscillator capabilities by adding possibilities for
modulation meaning that another signal (which is at a low frequency
of 0-10 Hz) influences some characteristic of our oscillator over
time. When we influence the amplitude of our signal that is called
a tremolo effect. Implement this effect to give our synthesizer a
bit more depth.
-}

modulate :: (Signal -> Signal) -> Oscillator -> Oscillator
modulate modF (Osc playEvent) = Osc (modF . playEvent)

tremolo :: Wave -> Hz -> Oscillator -> Oscillator
tremolo waveform frequency = modulate modF
  where
    modWave =
      tone'
        waveform
        frequency
        (sampleRate / fromIntegral (samplesPerPeriod frequency))
    modF = zipWith (*) (cycle modWave)
