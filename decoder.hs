-- 
-- module Decoder - Decode MP3.
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
-- TODO:
--
-- *) Better performance. Right now the decoder is not fast enough to decode
--    in real-time. (Biggest culprit right now is the Huffman decoding).
--
-- *) Check if frames with MixedBlocks are decoded correctly. This feature 
--    seems to be unusual.
--
-- *) Implement Intensity Stereo. Also unusual.
--
 
module Codec.Audio.MP3.Decoder (
   -- From MP3Unpack.hs
    mp3Seek
   ,mp3Unpack
   ,MP3Bitstream(..)
   -- From here
   ,mp3Decode
   ,MP3DecodeState(..)
   ,emptyMP3DecodeState
) where

import Codec.Audio.MP3.Unpack
import Codec.Audio.MP3.Types
import Codec.Audio.MP3.Tables
import Codec.Audio.MP3.HybridFilterBank

-- 
-- mp3Requantize
--
-- The heart of mp3 encoding is taking the 576 frequency lines and quantize
-- the values to remove irrelevant information, while keeping enough data to
-- keep noise to the minimum. This function does the reverse: it takes the
-- quantized integer frequency samples and requantizes them to a floating
-- point representation.
--
-- There are several parameters that affect the requantization:
--
-- gain, sg0, sg1, sg2 are "global" gains that affect the whole
-- frequency spectra, from 0-576. gain are for long blocks of 576 samples, 
-- and sg0-sg2 are for the three short blocks of 192 samples each.
--
-- There are also "local" gains, that only scale some frequency regions. The
-- 576 frequency regions are divided in 21 bands (for long blocks), scaled 
-- separately. The scale factors are saved in 'large' for long blocks and 
-- 'small' for small blocks.
--
-- *Codec.Audio.MP3.Tables> tableScaleBandIndexShort 32000
-- [(0,0),(0,0),(0,0),(0,0),(0,1),(0,1),(0,1),(0,1),(0,2),(0,2),(0,2),(0,2),
--  (1,0),(1,0),(1,0),(1,0),(1,1),(1,1),(1,1),(1,1),(1,2),(1,2),(1,2),(1,2),
--  (2,0),(2,0),(2,0),(2,0),(2,1),(2,1),(2,1),(2,1),(2,2),(2,2),(2,2),(2,2),
--  (3,0),(3,0),(3,0),(3,0),...
-- 
mp3Requantize :: SampleRate -> MP3DataChunk -> [Frequency]
mp3Requantize samplerate (MP3DataChunk _ bf gain (sg0, sg1, sg2) 
                         longsf shortsf _ compressed)
    | bf == LongBlocks  = long
    | bf == ShortBlocks = short
    | bf == MixedBlocks = take 36 long ++ drop 36 short
    where 
        long  = zipWith procLong  compressed longbands
        short = zipWith procShort compressed shortbands

        procLong sample sfb = 
            let localgain   = longsf !! sfb
                dsample     = fromIntegral sample
            in gain * localgain * dsample **^ (4/3)

        procShort sample (sfb, win) =
            let localgain = (shortsf !! sfb) !! win
                blockgain = case win of 0 -> sg0
                                        1 -> sg1
                                        2 -> sg2
                dsample   = fromIntegral sample
            in gain * localgain * blockgain * dsample **^ (4/3)
                                
        -- Frequency index (0-575) to scale factor band index (0-21).
        longbands = tableScaleBandIndexLong samplerate
        -- Frequency index to scale factor band index and window index (0-2).
        shortbands = tableScaleBandIndexShort samplerate


-- b **^ e == sign(b) * abs(b)**e
(**^) :: (Floating a, Ord a) => a -> a -> a
b **^ e = let sign = if b < 0 then -1 else 1
              b'   = abs b
          in sign * b' ** e
infixr 8 **^


--
-- mp3Reorder
--
-- The MP3 encoder reorders Short granules before Huffman coding for better
-- compression ratio. There are actually two different reorderings;
-- this function does both at the same time.
--
-- The Huffman decoded samples (chunkData) are ordered first by 
-- scale factor band, then by 192-granule, then by frequency. So if the
-- first scale factor band has size 4, the first 13 samples are
-- a[0],a[1],a[2],a[3],b[0],b[1],b[2],b[3],c[0],c[1],c[2],c[3],a[4],...
--  where a,b,c are the three 192-granules.
-- As samples within bands are quantized together, this gives a good
-- compression by the Huffman coder.
--
-- As the hybrid filter bank works on 18 samples a time, and the scale
-- factor bands don't have this fixed size, the samples are reordered to
-- work with the filter banks. This can be thought of as two reorderings:
-- First the samples are interleaved, ordered strictly by frequency:
-- a[0],b[0],c[0],a[1],b[1],c[1],... This ensures the hybrid filter bank
-- can transform it back to the time domain, 18 samples a time.
--
-- Then these samples are reordered again so the short IMDCT can be
-- performed on consecutive blocks of six samples. _This is strictly
-- for convienience_. If we don't do this second reordering, the
-- three short IMDCT's have to be done like this in the hybrid filter
-- bank:
--     imdct 6 [f!!0,f!!3,f!!6,f!!9,f!!12,f!!15] 
--     imdct 6 [f!!1,f!!4,f!!7,f!!10,f!!13,f!!16]
--     imdct 6 [f!!2,f!!5,f!!8,f!!11,f!!14,f!!17]
-- After the seceond reordering, we can instead split "f" into three equal
-- parts, and do
--     imdct 6 f0
-- for example.
--
mp3Reorder :: SampleRate -> BlockFlag -> [Frequency] -> [Frequency]
mp3Reorder sr bf freq 
    | bf == LongBlocks  = freq
    | bf == ShortBlocks = freq'
    | bf == MixedBlocks = take 36 freq ++ drop 36 freq'
    where
        -- We want the output list to be as long as the input list to 
        -- correctly handle IS Stereo decoding, but the unsafe reorderList 
        -- requires the input to be as long as the index list.
        inlen  = length freq
        freq'  = take inlen $ reorderList rtable (padWith 576 0.0 freq)
        rtable = tableReorder sr


-- 'reorderList' takes a list of indices and a list of elements and
-- reorders the list based on the indices. Example:
-- reorderList [1,2,0] [a,b,c] == [b,c,a]
-- UNSAFE, SLOW
reorderList :: [Int] -> [a] -> [a]
reorderList indices list = map (list !!) indices


-- Pads a list until it's length is n.
-- padWith 5 0 [1,2,3] == [1,2,3,0,0]
padWith :: Int -> a -> [a] -> [a]
padWith n padding xs     = xs ++ replicate (n - length xs) padding


-- 
-- mp3StereoMS
--
-- When two channels are similiar, the encoder can transmit the sum (M)
-- and the difference (S) of the two channels instead of the two independent
-- channels. In these cases, the sum signal will contain much more 
-- information than the difference signal. This is lossless.
--
mp3StereoMS :: [Frequency] -> [Frequency] -> ([Frequency], [Frequency])
mp3StereoMS middle side =
    let sqrtinv = 1 / (sqrt 2)
        left  = zipWith0 (\x y -> (x+y)*sqrtinv) 0.0 middle side
        right = zipWith0 (\x y -> (x-y)*sqrtinv) 0.0 middle side
    in (left, right)


zipWith0 :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
zipWith0 f pad (a:as) (b:bs) = f a   b : zipWith0 f pad as bs
zipWith0 f pad []     (b:bs) = f pad b : zipWith0 f pad [] bs
zipWith0 f pad (a:as) []     = f a pad : zipWith0 f pad as []
zipWith0 _ _   _      _      = []


-- 
-- mp3StereoIS
--
-- Another Joint Stereo mode.
--
-- TODO: Not implemented yet - unusual.
--
mp3StereoIS :: BlockFlag -> [Int] -> [[Int]] -> 
               [Frequency] -> [Frequency] -> ([Frequency], [Frequency])
mp3StereoIS _ _ _ left right = (left, right)
{-
mp3StereoIS bf long short left right = helper bf
    where
        helper MixedBlocks = (left, right) -- TODO.
        helper LongBlocks  = (left, right)
        helper ShortBlocks = (left, right)

        procOne 7 sl sr = (sl, sr)
        procOne 6 sl sr = (sl, 0.0)
        procOne p sl sr = let ratio  = tan $ ((fromIntegral p) * pi) / 12.0
                              ratiol = ratio / (1.0 + ratio)
                              ratior = 1.0   / (1.0 + ratio)
                              sl'    = ratiol * sl
                              sr'    = ratior * sr
                          in (sl', sr')
-}

-- 
-- mp3Decode
--
-- Decode the MP3.
--
-- Return value:
--
-- 1152 samples (-1.0 - 1.0) for the left and right channel.
--
-- TODO: Shouldn't need ~50 lines for this.
--
mp3Decode :: MP3DecodeState -> MP3Data -> (MP3DecodeState, [Sample], [Sample])
mp3Decode decstate chunks = decode chunks
  where
    decode (MP3Data2Channels sr ch (ms, is) gr0ch0 gr0ch1 gr1ch0 gr1ch1) =
        let s00                 = step0 gr0ch0 sr -- granule 0 begin
            s01                 = step0 gr0ch1 sr
            (s00',s01')         = decodeStereo ch gr0ch0 s00 gr0ch1 s01
            (state00, output00) = step1 gr0ch0 s00' (decodeState0 decstate)
            (state01, output01) = step1 gr0ch1 s01' (decodeState1 decstate)
            s10                 = step0 gr1ch0 sr
            s11                 = step0 gr1ch1 sr
            (s10', s11')        = decodeStereo ch gr1ch0 s10 gr1ch1 s11
            (state10, output10) = step1 gr1ch0 s10' state00
            (state11, output11) = step1 gr1ch1 s11' state01
        in (MP3DecodeState state10 state11,
            output00 ++ output10, output01 ++ output11)
        where
            decodeStereo JointStereo chunkch0 ch0 chunkch1 ch1 = 
                let bf0            = chunkBlockFlag chunkch0
                    (isL, isS)     = chunkISParam chunkch0
                    (ch0', ch1')   = 
                        if ms then mp3StereoMS ch0 ch1 
                              else (ch0, ch1)
                    (ch0'', ch1'') = 
                        if is then mp3StereoIS bf0 isL isS ch0' ch1'
                              else (ch0', ch1')
                in  (ch0'', ch1'')
            decodeStereo _           _ ch0 _ ch1 = (ch0, ch1)

    decode (MP3Data1Channels sr _ _ gr0ch0 gr1ch0) =
        let s00               = step0 gr0ch0 sr
            s10               = step0 gr1ch0 sr
            (state0, output0) = step1 gr0ch0 s00 (decodeState0 decstate)
            (state1, output1) = step1 gr1ch0 s10 state0
            output            = output0 ++ output1
        in (MP3DecodeState state1 state1, output, output)

    -- At this point the audio samples are _not_ padded with the zeros
    -- from the zero huffman region. This is to make the IS stereo
    -- implementation less messy. (Of course, IS is not implemented as of
    -- version 0.0.1).
    step0 chunk sr = 
        let freq  = mp3Requantize sr chunk
            freq' = mp3Reorder    sr (chunkBlockFlag chunk) freq
        in freq'

    -- ... The hybrid filter bank will however pad.
    step1 chunk inp state =
        let bf = chunkBlockFlag chunk
            bt = chunkBlockType chunk
        in mp3HybridFilterBank bf bt state inp


data MP3DecodeState = MP3DecodeState {
    decodeState0 :: MP3HybridState,
    decodeState1 :: MP3HybridState
} 


emptyMP3DecodeState :: MP3DecodeState
emptyMP3DecodeState = MP3DecodeState emptyMP3HybridState emptyMP3HybridState

