{-# LANGUAGE ForeignFunctionInterface #-}
-- 
-- module SynthesisFilterBank - The MPEG synthesis filterbank.
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

module Codec.Audio.MP3.SynthesisFilterBank (
     MP3SynthState(..)
    ,mp3SynthesisFilterBank
) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe

foreign import ccall "c_synth.h synth"
    c_synth :: Ptr CDouble -> 
               Ptr CDouble -> 
               Ptr CDouble -> 
               Ptr CDouble -> 
               IO ()

synthIO :: [CDouble] -> [CDouble] -> IO ([CDouble], [CDouble])
synthIO state input
    = withArray state $ \cstate ->
      withArray input $ \cinput ->
      allocaArray 1024 $ \cstate' ->
      allocaArray 576  $ \coutput ->
      do c_synth cstate cinput cstate' coutput
         state' <- peekArray 1024 cstate'
         output <- peekArray 576 coutput
         return (state', output)

data MP3SynthState = MP3SynthState [Sample] deriving (Show)

type Sample = Double
mp3SynthesisFilterBank :: MP3SynthState -> [Sample] -> 
                          (MP3SynthState, [Sample])
mp3SynthesisFilterBank (MP3SynthState state) input =
    let cstate = map realToFrac state
        cinput = map realToFrac input
        (cstate', coutput) = unsafePerformIO (synthIO cstate cinput)
        state' = map realToFrac cstate'
        output = map realToFrac coutput
    in  (MP3SynthState state', output)

