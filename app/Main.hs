module Main (main) where

import           Git.Profile.Cli.Parser
import           Git.Profile.Run        (runApp)
import           RIO
import           System.IO              (hPutStrLn)

main :: IO ()
main = do
    commandLineOptions <- parseCommandLineOptions
        `catch` \(StringException e _) -> do
            hPutStrLn stderr e
            exitFailure
    runApp commandLineOptions
