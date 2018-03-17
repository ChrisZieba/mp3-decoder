-- 
-- module HybridFilterBank - Frequency to time.
-- 
-- This code is part of the Experimental Haskell MP3 Decoder, version 0.0.1.
-- Copyright (c) 2008 Bjorn Edstrom <be@bjrn.se>
--
-- This software is provided 'as-is', without any express or implied
-- warranty. In no event will the authors be held liable for any damages
-- arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
--
--    1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software. If you use this software
--    in a product, an acknowledgment in the product documentation would be
--    appreciated but is not required.
--
--    2. Altered source versions must be plainly marked as such, and must not be
--    misrepresented as being the original software.
--
--    3. This notice may not be removed or altered from any source
--    distribution.
--

module Codec.Audio.MP3.HybridFilterBank (
    mp3HybridFilterBank
   ,MP3HybridState(..)
   ,emptyMP3HybridState
) where

import Codec.Audio.MP3.IMDCT
import Codec.Audio.MP3.SynthesisFilterBank
import Codec.Audio.MP3.Tables
import Codec.Audio.MP3.Types

{-
mapBlock :: Int -> ([a] -> [b]) -> [a] -> [b]
mapBlock blocksize func []  = []
mapBlock blocksize func seq = 
    let (block, rem) = splitAt blocksize seq
    in func block ++ mapBlock blocksize func rem
-}

mapBlock :: Int -> ([a] -> b) -> [a] -> [b]
mapBlock blocksize func []  = []
mapBlock blocksize func seq =
    let (block, rem) = splitAt blocksize seq
    in func block : mapBlock blocksize func rem


-- 'windowWith' windows two signals. For readability, so we can do:
-- signal' = signal `windowWith` win
windowWith :: (Num a) => [a] -> [a] -> [a]
windowWith = zipWith (*)


-- 
-- mp3IMDCT
--
-- Input: 576 frequency samples and state from the last time the function
-- was called.
-- Output: 576 time domain samples and new state.
--
mp3IMDCT :: BlockFlag -> Int -> [Frequency] -> [Sample] -> ([Sample], [Sample])
mp3IMDCT blockflag blocktype freq overlap =
    let (samples, overlap') = 
            case blockflag of
                 LongBlocks  -> transf (doImdctLong blocktype) freq
                 ShortBlocks -> transf (doImdctShort) freq
                 MixedBlocks -> transf (doImdctLong 0)  (take 36 freq) <++>
                                transf (doImdctShort) (drop 36 freq)
        samples' = zipWith (+) samples overlap
    in (samples', overlap')
    where
        transf imdctfunc input = unzipConcat $ mapBlock 18 toSO input
            where
                -- toSO takes 18 input samples b and computes 36 time samples
                -- by the IMDCT. These are further divided into two equal
                -- parts (S, O) where S are time samples for this frame
                -- and O are values to be overlapped in the next frame.
                toSO b = splitAt 18 (imdctfunc b)
                unzipConcat xs = let (a, b) = unzip xs
                                 in (concat a, concat b)

--
-- doImdctLong, doImdctShort
--
-- IMDCT with windows. This also does the overlapping when short blocks
-- are used.
--
doImdctLong :: Int -> [Frequency] -> [Sample]
doImdctLong blocktype f = imdct 18 f `windowWith` tableImdctWindow blocktype


doImdctShort :: [Frequency] -> [Sample]
doImdctShort f = overlap3 shorta shortb shortc
  where
    (f1, f2, f3) = splitAt2 6 f
    shorta       = imdct 6 f1 `windowWith` tableImdctWindow 2
    shortb       = imdct 6 f2 `windowWith` tableImdctWindow 2
    shortc       = imdct 6 f3 `windowWith` tableImdctWindow 2
    --shorta = imdct 6 [f!!0,f!!3,f!!6,f!!9,f!!12,f!!15] `windowWith` tableImdctWindow 2
    --shortb = imdct 6 [f!!1,f!!4,f!!7,f!!10,f!!13,f!!16] `windowWith` tableImdctWindow 2
    --shortc = imdct 6 [f!!2,f!!5,f!!8,f!!11,f!!14,f!!17] `windowWith` tableImdctWindow 2

    overlap3 a b c = 
      p1 ++ (zipWith3 add3 (a ++ p2) (p1 ++ b ++ p1) (p2 ++ c)) ++ p1
      where
        add3 x y z = x+y+z
        p1         = [0,0,0, 0,0,0]
        p2         = [0,0,0, 0,0,0, 0,0,0, 0,0,0]


splitAt2 :: Int -> [a] -> ([a], [a], [a])
splitAt2 n xs = let (part1, part23) = splitAt n xs
                    (part2, part3)  = splitAt n part23
                in (part1, part2, part3)

(<++>) :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
(as, xs) <++> (bs, ys) = (as++bs, xs++ys)
infixr 5 <++>


-- 
-- mp3AA
--
-- Undo the encoders alias reduction.
--
-- TODO: Rewrite this, clumsy and non-intuitive.
--
mp3AA :: BlockFlag -> Int -> [Frequency] -> [Frequency]
mp3AA blockflag blocktype freq
  | blocktype == 2 && blockflag /= MixedBlocks   = freq
  | blocktype == 2 && blockflag == MixedBlocks   = 
      (take 9 freq) ++ aaHelper (take  18 (drop 9 freq)) ++ (drop  27 freq)
  | otherwise                                    = 
      (take 9 freq) ++ aaHelper (take 558 (drop 9 freq)) ++ (drop 567 freq)
  where
      aaHelper []    = []
      aaHelper chunk = before ++ aaButterfly middle ++ after ++ 
                       aaHelper (drop 18 chunk)
          where
              before = take 1 chunk
              middle = take 16 (drop 1 chunk)
              after  = take 1 (drop 17 chunk)
      aaButterfly f = zipWith (-) (take 8 seqcs) (take 8 seqca) ++
                      zipWith (+) (drop 8 seqcs) (drop 8 seqca)
          where
              seqcs = zipWith (*) f (reverse cs ++ cs)
              seqca = reverse $ zipWith (*) f (reverse ca ++ ca)
      cs = [1 / sqrt (1.0 + c**2) | c <- aaCoeff]
      ca = [c / sqrt (1.0 + c**2) | c <- aaCoeff]
      aaCoeff = [-0.6, -0.535, -0.33, -0.185, 
                 -0.095, -0.041, -0.0142, -0.0037]


{-
mp3AliasCancel :: BlockFlag -> [Frequency] -> [Frequency]
mp3AliasCancel blockflag freq
  | blockflag == ShortBlocks = freq
  | blockflag == LongBlocks  = helper 31
  | blockflag == MixedBlocks = helper 1
  where
    butterflies samples = let samplesa = zipWith (*) samples cs'
                              samplesb = zipWith (*) (reverse samples) ca'
                          in zipWith (+) samplesa samplesb
   -- Coeffs.
   cs' = reverse cs ++ cs
   ca' = map (-) (reverse ca) ++ ca
   cs = [1 / sqrt (1.0 + c**2) | c <- aaCoeff]
   ca = [c / sqrt (1.0 + c**2) | c <- aaCoeff]
   aaCoeff = [-0.6, -0.535, -0.33, -0.185, -0.095, -0.041, -0.0142, -0.0037]
-}

-- 
-- mp3FrequencyInvert
--
-- The time samples returned from mp3IMDCT are inverted. This is the same
-- as with bandpass sampling: odd subbands have inverted frequency spectra -
-- invert it by changing signs on odd samples.
--
mp3FrequencyInvert :: [Sample] -> [Sample]
mp3FrequencyInvert = zipWith (*) pattern
    where
        pattern = cycle $ replicate 18 1 ++ take 18 (cycle [1,-1])


-- Pads a list until it's length is n.
-- padWith 5 0 [1,2,3] == [1,2,3,0,0]
padWith :: Int -> a -> [a] -> [a]
padWith n padding xs     = xs ++ replicate (n - length xs) padding


-- 
-- mp3HybridFilterBank
--
-- Frequency domain to time domain.
--
mp3HybridFilterBank :: BlockFlag -> Int -> 
                       MP3HybridState -> [Frequency] -> 
                       (MP3HybridState, [Sample])
mp3HybridFilterBank bf bt (MP3HybridState simdct ssynthesis) input =
    let input'                = padWith 576 0.0 input -- ensure length 576
        aa                    = mp3AA    bf bt input'
        (samp, simdct')       = mp3IMDCT bf bt aa simdct
        samp'                 = mp3FrequencyInvert samp

        --(ssynthesis', output) = (ssynthesis, take 18 samp) -- (See Driver.hs)
        (ssynthesis', output) = mp3SynthesisFilterBank ssynthesis samp'

    in (MP3HybridState simdct' ssynthesis', output)


-- [Sample] = IMDCT output from previous granule, used for overlapping.
-- MP3SynthState = State for the synthesis filterbank.
data MP3HybridState = MP3HybridState [Sample] MP3SynthState

emptyMP3HybridState :: MP3HybridState
emptyMP3HybridState = MP3HybridState (replicate 576 0.0) 
                                     (MP3SynthState (replicate 1024 0.0))

