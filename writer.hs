-- 
-- module PCMWriter - For writing PCM WAV files.
-- This is a small simple (and very SLOW) library for demonstrating
-- the decoder. For more powerful manipulations of PCM
-- WAV, see HSoundFile at Hackage.
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

module PCMWriter (
     writeHeader
    ,writeSamplerate
    ,writeSamples
) where

import Data.Word
import Data.Bits
import System.IO
import Char
import Control.Monad
import Control.Monad.State

type PCMSample = Word16

fracToPCM :: RealFrac a => a -> PCMSample
fracToPCM x
    | y <= (-32767)   = fromIntegral (-32767)
    | y >= 32767      = fromIntegral 32767
    | otherwise       = fromIntegral y
    where
        y = round (x * 32767.0)

class AudioSampleRepr a where
    toPcmRepr   :: a -> PCMSample

instance AudioSampleRepr Double where
    toPcmRepr = fracToPCM

instance AudioSampleRepr Float where
    toPcmRepr = fracToPCM

-- Add other instances here.

from16bit n = [chr . fromIntegral $ n .&. 0xff, 
               chr . fromIntegral $ n `shiftR` 8]
from32bit n = [chr . fromIntegral $ n .&. 0xff,
               chr . fromIntegral $ (n `shiftR` 8) .&. 0xff,
               chr . fromIntegral $ (n `shiftR` 16) .&. 0xff,
               chr . fromIntegral $ (n `shiftR` 24) .&. 0xff]

hWrite16 :: Handle -> Word16 -> IO ()
hWrite16 handle n = do hPutStr handle (from16bit n)

hWrite32 :: Handle -> Word32 -> IO ()
hWrite32 handle n = do hPutStr handle (from32bit n)

writeSamplerate :: Handle -> Int -> IO ()
writeSamplerate handle sr = 
    do cur <- hTell handle
       hSeek    handle AbsoluteSeek 24
       hWrite32 handle (fromIntegral sr)
       hWrite32 handle (fromIntegral sr * 2 * 2)
       hSeek    handle AbsoluteSeek cur

writeNumSamples :: Handle -> Int -> IO ()
writeNumSamples handle num =
    do cur <- hTell handle
       let count1 = (num * 2 * 2) 
           count2 = 36 + count1
       hSeek    handle AbsoluteSeek 4
       hWrite32 handle (fromIntegral count2)
       hSeek    handle AbsoluteSeek 40
       hWrite32 handle (fromIntegral count1)
       hSeek    handle AbsoluteSeek cur

writeHeader :: Handle -> IO ()
writeHeader handle = 
    do  write "RIFF"
        write "\xff\xff\xff\xff"
        write "WAVEfmt "
        write32 16
        write16 1
        write16 2                -- Num. channels
        write32 44100            -- Sample rate
        write32 (44100 * 2 * 2)  -- Derived from sample rate.
        write16 4
        write16 16
        write "data"
        write "\xff\xff\xff\xff" -- num samples
    where
        write   = do hPutStr handle
        write16 = do hWrite16 handle
        write32 = do hWrite32 handle

writeSamples :: AudioSampleRepr a => Handle -> [a] -> [a] -> IO ()
writeSamples handle ch0 ch1 =
    do zipWithM_ writeInterleaved ch0 ch1
       cur <- hTell handle
       let totalsamples = fromIntegral $ (cur - 44) `div` (2*2)
       writeNumSamples handle totalsamples
       --hFlush handle
    where
        writeInterleaved ch0sample ch1sample = 
            do hWrite16 handle (toPcmRepr ch0sample)
               hWrite16 handle (toPcmRepr ch1sample)


