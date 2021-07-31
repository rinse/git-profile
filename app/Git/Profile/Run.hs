{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.Run (runApp) where

import           Control.Monad.Cont
import qualified Data.Text                          as T
import           Git.Profile.App.RunSwitch          (runSwitch)
import           Git.Profile.App.SwitchEnv
import           Git.Profile.Cli.CommandLineOptions
import           Git.Profile.GitProfile             (envOrDefaultGitProfilePath)
import           RIO

runApp :: CommandLineOptions -> IO ()
runApp (SwitchCmd (SwitchArguments profileName profileFilePathMaybe)) =
    flip runContT return $ do
        logOptions <- logOptionsHandle stderr False
        logFunc <- ContT $ withLogFunc logOptions
        profileFilePath <- maybe (T.pack <$> envOrDefaultGitProfilePath) pure profileFilePathMaybe
        let env = Env
                    { envProfileName = profileName
                    , envProfileFilePath = profileFilePath
                    , envLogFunc = logFunc
                    }
        runRIO env runSwitch
