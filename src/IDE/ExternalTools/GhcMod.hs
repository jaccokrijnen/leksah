{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.ExternalTools.GhcMod
-- Copyright   :  2007-2014 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.ExternalTools.GhcMod where

import Control.Concurrent
import System.Process
import qualified Data.Text as T
import Data.Text (Text)
import System.IO
import GHC.IO.Handle
import Data.Function

data GhcMod = GhcMod {
    process :: ProcessHandle,
    hin  :: Handle,
    hout :: Handle,
    herr :: Handle
}



startGhcMod  :: Maybe FilePath -> IO GhcMod
startGhcMod mbDir = do
    (Just hin, Just hout, Just herr, process) <- createProcess (proc "ghc-mod" ["legacy-interactive"])
        { std_in  = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          cwd = mbDir }

    return (GhcMod process hin hout herr)


runGhcModCommand ::
    GhcMod ->
    Text ->
    ([Text] -> IO ()) ->
    IO ()
runGhcModCommand ghcMod command callback = do
    forkIO $ do
        hPutStrLn (hin ghcMod) (T.unpack command)
        hFlush (hin ghcMod)
        lines <- fix $ \loop -> do
                     line <- T.pack <$> hGetLine (hout ghcMod)
                     (:) <$> return line <*> if line == "OK" then return [] else loop
        callback lines

    return ()

killGhcMod :: GhcMod -> IO ()
killGhcMod ghcMod  = do hPutStrLn (hin ghcMod) "" >> hFlush (hin ghcMod) -- terminateProcess . process

--newGhcMod :: FilePath -> IO ToolState
--newGhcMod path = do
--    toolState <- newToolState
--    runInteractiveTool toolState ghcModCommandLineReader "ghc-mod" ["legacy-interactive"] Nothing
--    return toolState
--
--
--ghcModCommandLineReader :: CommandLineReader
--ghcModCommandLineReader = CommandLineReader {
--    initialCommand = Nothing,
--    parseInitialPrompt = return "",
--    parseFollowingPrompt = return "",
--    errorSyncCommand = Nothing,
--    parseExpectedError = undefined,
--    outputSyncCommand = Nothing,
--    isExpectedOutput = undefined
--}
