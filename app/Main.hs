module Main where

import           Control.Exception.Safe
import           Git.Profile.Command.Command
import           Git.Profile.Command.Parser
import           System.Exit                 (exitFailure)
import           System.IO                   (hPutStrLn, stderr)

main :: IO ()
main =
    (parseCommand >>= runCommand)
    `catch` \(StringException e _) -> do
        hPutStrLn stderr e
        exitFailure
