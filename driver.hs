module Main where

import qualified Data.ByteString as B
import System.IO
import System

import qualified PCMWriter

import Codec.Audio.MP3.Decoder
import Codec.Audio.MP3.Types

mainLoopX :: B.ByteString -> Handle -> IO ()
mainLoopX contents outf =  do loopInit (MP3Bitstream contents []) 
                                       emptyMP3DecodeState
    where
        loopInit bitstream decstate = helper (mp3Seek bitstream)
            where
                helper Nothing           = error "Not an MP3."
                helper (Just bitstream') = doUnpack bitstream' decstate 0

        doUnpack bitstream decstate cnt = helper (mp3Unpack bitstream)
            where
                helper (bitstream', Nothing) = 
                    do putStrLn $ "Skipped broken frame " ++ (show cnt)
                       doUnpack bitstream' emptyMP3DecodeState (cnt+1)

                helper (bitstream', Just mp3data) = 
                    do let (decstate', ch0, ch1) = mp3Decode decstate mp3data
                           sr = mp3dataSampleRate mp3data
                       
                       putStrLn $ "Decoded frame " ++ (show cnt) ++ ": " ++ prettyData mp3data
                       
                       -- To the reader, that is, you: For fun, change mp3HybridFilterBank
                       -- so it doesn't do the synthesis step, and only returns the 18
                       -- first samples. Then change the sr to sr/32.
                       PCMWriter.writeSamplerate outf sr -- (sr `div` 32)
                       PCMWriter.writeSamples outf ch0 ch1
                       doUnpack bitstream' decstate' (cnt+1)
       
main :: IO ()
main = do args     <- getArgs
          contents <- B.readFile (head args)
          outh     <- openBinaryFile "out.wav" WriteMode
          PCMWriter.writeHeader outh
          mainLoopX contents outh

