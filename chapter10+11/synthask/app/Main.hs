module Main (main) where

import qualified Codec.Wav
import Composition.Note
import Composition.Notelength
import Composition.Performance
import Composition.Pitch
import Data.Array.Unboxed (listArray)
import Data.Audio
import Data.Foldable
import Data.Int
import Sound.Sampler
import Sound.Sound
import Sound.Synth
import Util.Types
import Prelude

limit :: Signal -> Signal
limit = map (min threshold . max (-threshold) . (* threshold))
  where
    threshold = 0.9

signalToSampleData :: Signal -> SampleData Int16
signalToSampleData signal =
  listArray (0, n) $ map fromSample signal
  where
    n = length signal - 1

writeWav :: FilePath -> Signal -> IO ()
writeWav filePath signal = do
  putStrLn $
    "Writing " ++ show (length signal) ++ " samples to " ++ filePath
  let sampleData = signalToSampleData $ limit signal
      audio =
        Audio
          { Data.Audio.sampleRate = round Util.Types.sampleRate,
            channelNumber = 1,
            sampleData = sampleData
          }
  Codec.Wav.exportFile filePath audio

main :: IO ()
main = do
  sampler <- readSamples
  writeWav "composition.wav" $ mkSignal sampler
  where
    readSamples :: IO Sampler
    readSamples = do
      kickAudio <- wav "../samples/kick.wav" :: IO (Audio Int16)
      snareAudio <- wav "../samples/snare.wav" :: IO (Audio Int16)
      hiHatAudio <- wav "../samples/hiHat.wav" :: IO (Audio Int16)
      return $
        mkSampler
          [ kick =: kickAudio,
            snare =: snareAudio,
            hihat =: hiHatAudio
          ]

    kick = c 2
    snare = c 3
    hihat = c 4

    drumNotes :: NoteStructure
    drumNotes =
      ( ((kick .| hn <~> snare .| hn) <~| 4)
          <:> ((hihat .| qn) <~| 16)
      )
        <~| 4

    plays osci = play osci . toPerformance (TempoInfo 120 2)

    mkSignal :: Sampler -> Signal
    mkSignal sampler = intro ++ mainPart ++ outro
      where
        intro =
          mix
            [ pluck `plays` (p wn <~> (mainMelody <~| 2)),
              sampler `plays` drumNotes
            ]

        mainPart =
          mix
            [ pluck `plays` (mainMelody <~| 2),
              sampler `plays` drumNotes,
              piano `plays` (chords <~| 2),
              bass `plays` (bassMelody <~| 2)
            ]

        outro =
          mix
            [ pluck `plays` outroMelody,
              piano `plays` outroMelody,
              bass `plays` outroMelody
            ]

chords :: NoteStructure
chords =
  (gs 3 .| (2 * wn) <:> c 4 .| (2 * wn) <:> ds 4 .| (2 * wn))
    <~> (b 3 .| (2 * wn) <:> ds 4 .| (2 * wn) <:> fs 4 .| (2 * wn))
    <~> (cs 4 .| (2 * wn) <:> f 4 .| (2 * wn) <:> gs 4 .| (2 * wn))
    <~> (cs 4 .| (2 * wn) <:> fs 4 .| (2 * wn) <:> as 4 .| (2 * wn))

bassMelody :: NoteStructure
bassMelody =
  gs 1 .| (2 * wn)
    <~> b 1 .| (2 * wn)
    <~> cs 2 .| (2 * wn)
    <~> fs 2 .| (2 * wn)

mainMelody :: NoteStructure
mainMelody =
  gs 4 .| qn
    <~> cs 5 .| qn
    <~> ds 5 .| qn
    <~> fs 5 .| qn
    <~> as 5 .| qn
    <~> p en
    <~> gs 5 .| hn
    <~> p en
    <~> fs 5 .| qn
    <~> f 5 .| qn
    <~> ds 5 .| qn
    <~> cs 5 .| qn
    <~> cs 5 .| en
    <~> ds 5 .| qn
    <~> ds 5 .| qn
    <~> gs 6 .| sn
    <~> fs 6 .| sn
    <~> cs 6 .| sn
    <~> ds 6 .| en
    <~> p sn
    <~> gs 4 .| qn
    <~> cs 5 .| qn
    <~> ds 5 .| qn
    <~> fs 5 .| qn
    <~> as 5 .| qn
    <~> p en
    <~> gs 5 .| hn
    <~> p en
    <~> gs 5 .| en
    <~> gs 5 .| en
    <~> ds 5 .| qn
    <~> fs 5 .| qn
    <~> gs 5 .| en
    <~> fs 5 .| en
    <~> gs 5 .| en
    <~> p en
    <~> fs 5 .| en
    <~> gs 5 .| en
    <~> p en
    <~> b 5 .| en
    <~> as 5 .| en
    <~> gs 5 .| en

outroMelody :: NoteStructure
outroMelody =
  gs 4 .| sn
    <~> fs 4 .| sn
    <~> gs 4 .| sn
    <~> b 4 .| sn
    <~> gs 4 .| sn
    <~> b 4 .| sn
    <~> cs 5 .| sn
    <~> b 4 .| sn
    <~> cs 5 .| sn
    <~> ds 5 .| sn
    <~> cs 5 .| sn
    <~> ds 5 .| sn
    <~> fs 5 .| sn
    <~> ds 5 .| sn
    <~> fs 5 .| sn
    <~> gs 5 .| sn
    <~> b 5 .| sn
    <~> gs 5 .| sn
    <~> b 5 .| sn
    <~> gs 5 .| sn
    <~> fs 5 .| sn
    <~> gs 5 .| sn
    <~> fs 5 .| sn
    <~> ds 5 .| sn
    <~> fs 5 .| sn
    <~> ds 5 .| sn
    <~> cs 5 .| sn
    <~> ds 5 .| sn
    <~> cs 5 .| sn
    <~> b 4 .| sn
    <~> cs 5 .| sn
    <~> b 4 .| sn
    <~> p hn
