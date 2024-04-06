{-# LANGUAGE FlexibleContexts #-}

module Sound.Sampler
  ( SampleMapping,
    SampleMap,
    Sampler (..),
    mkSampler,
    (=:),
    wav,
    getSample,
  )
where

import qualified Codec.Wav
import Composition.Notelength (Seconds)
import Composition.Pitch (Hz, Pitchable, toFrequency)
import Data.Array.Base (MArray)
import Data.Array.IO (IOUArray)
import Data.Array.Unboxed (IArray, UArray, elems)
import Data.Audio
import qualified Data.Map as M
import Sound.Synth (silence)
import Util.Types

type SampleMapping = (Hz, Signal)

newtype SampleMap = SampleMap (M.Map Hz Signal)

newtype Sampler = Sampler
  { sampleMap :: SampleMap
  }

mkSampler :: [SampleMapping] -> Sampler
mkSampler mapping = Sampler $ SampleMap sampleMap
  where
    sampleMap = foldr (\(k, v) acc -> M.insert k v acc) M.empty mapping

(=:) :: (Pitchable p, Audible a, IArray UArray a) => p -> Audio a -> SampleMapping
(=:) pitch audio = (toFrequency pitch, sample)
  where
    sample :: Signal
    sample = signalFromSampleData $ sampleData audio

    signalFromSampleData :: (Audible a, IArray UArray a) => SampleData a -> Signal
    signalFromSampleData = map toSample . elems

wav ::
  ( MArray IOUArray a IO,
    IArray UArray a,
    Audible a,
    Codec.Wav.AudibleInWav a
  ) =>
  FilePath ->
  IO (Audio a)
wav filepath = do
  mAudio <- Codec.Wav.importFile filepath
  return $ either error checkSampleRate mAudio
  where
    checkSampleRate :: Audio a -> Audio a
    checkSampleRate audio
      | fromIntegral (Data.Audio.sampleRate audio) == Util.Types.sampleRate = audio
      | otherwise =
          error $
            "Samplerate of "
              ++ filepath
              ++ " ("
              ++ show (Data.Audio.sampleRate audio)
              ++ "Hz) "
              ++ "does not match project sample rate of "
              ++ show Util.Types.sampleRate
              ++ "Hz!"

getSample :: SampleMap -> Hz -> Seconds -> Signal
getSample (SampleMap sampleMap) freq duration =
  case M.lookup freq sampleMap of
    Nothing -> silence duration
    Just audio -> take (samplesPerSecond duration) audio
