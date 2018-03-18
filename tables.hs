{-# LANGUAGE ForeignFunctionInterface #-}
-- 
-- module IMDCT
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
module Codec.Audio.MP3.IMDCT (
    imdct
) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe


foreign import ccall "c_imdct.h imdct"
    c_imdct :: CInt -> 
               Ptr CDouble -> 
               Ptr CDouble -> 
               IO ()

imdctIO :: Int -> [CDouble] -> IO [CDouble]
imdctIO points input
    = withArray   input      $ \cinput ->
      allocaArray (points*2) $ \coutput ->
      do c_imdct (fromIntegral points) cinput coutput
         peekArray (points*2) coutput


imdct :: Int -> [Double] -> [Double]
imdct points input = let cinput  = map realToFrac input
                         coutput = unsafePerformIO (imdctIO points cinput)
                         output  = map realToFrac coutput
                     in output

