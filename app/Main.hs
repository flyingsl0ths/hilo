module Main where

import qualified Data.ByteString as BS
import qualified Data.Word as DW
import qualified System.Posix.ByteString as PSB
import qualified System.Posix.Terminal as PT

charToWord8 :: Char -> DW.Word8
charToWord8 = toEnum . fromEnum

enterRawMode :: IO PT.TerminalAttributes
enterRawMode =
  do
    attrs <- PT.getTerminalAttributes PSB.stdInput
    let attrs' = foldl PT.withoutMode attrs [PT.EnableEcho, PT.ProcessInput]
    PT.setTerminalAttributes PSB.stdInput attrs' PT.WhenFlushed
    return attrs

leaveRawMode :: PT.TerminalAttributes -> IO ()
leaveRawMode origin = PT.setTerminalAttributes PSB.stdInput origin PT.WhenFlushed

main :: IO ()
main =
  do
    origin <- PT.getTerminalAttributes PSB.stdInput
    _ <- enterRawMode

    bs <-
      PSB.fdRead
        PSB.stdInput
        1

    if BS.head bs == charToWord8 'q'
      then leaveRawMode origin
      else main
