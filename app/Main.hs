{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Word as DW
import qualified System.Posix.ByteString as PSB

firstOf :: BS.ByteString -> Maybe DW.Word8
firstOf = maybe Nothing (Just . fst) . BS.uncons

charToWord8 :: Char -> DW.Word8
charToWord8 = toEnum . fromEnum

main :: IO ()
main = read'
 where
  read' =
    do
      bs <-
        PSB.fdRead
          PSB.stdInput
          1
      case firstOf bs of
        Just q
          | q == charToWord8 'q' -> return ()
          | otherwise -> read'
        _ -> read'
