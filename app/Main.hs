module Main where

import           Control.Monad.State
import qualified Data.ByteString         as BS
import           Data.Char               (isControl, ord)
import qualified Data.Word               as DW
import qualified System.Posix.ByteString as PSB
import qualified System.Posix.Terminal   as PT

charToWord8 :: Char -> DW.Word8
charToWord8 = toEnum . fromEnum

word8ToChar :: DW.Word8 -> Char
word8ToChar = toEnum . fromIntegral

data EditorState = EditorState
                     { originTermAttrs :: PT.TerminalAttributes
                     }

type EditorM = StateT EditorState IO

enterRawMode :: IO PT.TerminalAttributes
enterRawMode =
  do
    attrs <- PT.getTerminalAttributes PSB.stdInput
    let attrs' =
          foldl
            PT.withoutMode
            attrs
            [ PT.EnableEcho
            , PT.ProcessInput
            , PT.KeyboardInterrupts
            , PT.StartStopOutput
            ]
    PT.setTerminalAttributes PSB.stdInput attrs' PT.WhenFlushed
    return attrs

deinitEditor :: EditorM ()
deinitEditor =
  do
    origin <- get
    liftIO $ leaveRawMode origin
 where
  leaveRawMode (EditorState origin) =
    liftIO $
      PT.setTerminalAttributes
        PSB.stdInput
        origin
        PT.WhenFlushed

initEditor :: IO EditorState
initEditor =
  do
    origin <- PT.getTerminalAttributes PSB.stdInput
    _ <- enterRawMode
    return EditorState{originTermAttrs = origin}

runEditor :: EditorM ()
runEditor =
  do
    bs <-
      liftIO $
        PSB.fdRead
          PSB.stdInput
          1
    let top' = word8ToChar $ BS.head bs

    if isControl $ top'
      then liftIO . putStrLn $ show $ fromEnum top'
      else liftIO . putStrLn $ (show . ord $ top') <> " ('" <> (top' : "") <> "')"

    when (top' /= 'q') runEditor

main :: IO ()
main =
  do
    st <- initEditor
    evalStateT (runEditor >> deinitEditor) st
