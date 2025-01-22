module Main where

import qualified System.Posix.ByteString as PSB

main :: IO ()
main =
  do
    bs <- PSB.fdRead PSB.stdInput 1
    print bs
    main
