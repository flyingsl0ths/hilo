module Main where

import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.Word as DW
import qualified System.Posix.ByteString as PSB
import qualified System.Posix.Terminal as PT

charToWord8 :: Char -> DW.Word8
charToWord8 = toEnum . fromEnum

data EditorState = EditorState
    {originTermAttrs :: PT.TerminalAttributes}

type EditorM = StateT EditorState IO

enterRawMode :: IO PT.TerminalAttributes
enterRawMode =
    do
        attrs <- PT.getTerminalAttributes PSB.stdInput
        let attrs' =
                foldl
                    PT.withoutMode
                    attrs
                    [PT.EnableEcho, PT.ProcessInput]
        PT.setTerminalAttributes PSB.stdInput attrs' PT.WhenFlushed
        return attrs

deinitEditor :: EditorM ()
deinitEditor =
    do
        origin <- gets originTermAttrs
        liftIO $ leaveRawMode origin
  where
    leaveRawMode origin' =
        PT.setTerminalAttributes
            PSB.stdInput
            origin'
            PT.WhenFlushed

initEditor :: IO EditorState
initEditor =
    do
        origin <- PT.getTerminalAttributes PSB.stdInput
        _ <- liftIO $ enterRawMode
        return EditorState {originTermAttrs = origin}

runEditor :: EditorM ()
runEditor =
    do
        bs <-
            liftIO $
                PSB.fdRead
                    PSB.stdInput
                    1

        when (BS.head bs /= charToWord8 'q') runEditor

main :: IO ()
main =
    do
        st <- initEditor
        evalStateT (runEditor >> deinitEditor) st
