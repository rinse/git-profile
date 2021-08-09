{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.Run (runApp) where

import           Control.Monad.Cont
import qualified Data.Text                          as T
import           Git.Profile.App.RunSave            (runSave)
import           Git.Profile.App.RunSwitch          (runSwitch)
import qualified Git.Profile.App.SaveEnv            as Save
import qualified Git.Profile.App.SwitchEnv          as Switch
import           Git.Profile.Cli.CommandLineOptions
import           Git.Profile.GitProfile             (envOrDefaultGitProfilePath)
import           RIO

runApp :: CommandLineOptions -> IO ()
runApp (SwitchCmd (SwitchArguments profileName profileFilePathMaybe)) =
    flip runContT return $ do
        profileFilePath <- maybe (T.pack <$> envOrDefaultGitProfilePath) pure profileFilePathMaybe
        logFunc <- logOptionsHandle stderr False >>= ContT . withLogFunc
        let env = Switch.Env
                    { envProfileName = profileName
                    , envProfileFilePath = profileFilePath
                    , envLogFunc = logFunc
                    }
        runRIO env runSwitch
runApp (SaveCmd (SaveArguments profileName configName profileFilePathMaybe)) =
    flip runContT return $ do
        profileFilePath <- maybe (T.pack <$> envOrDefaultGitProfilePath) pure profileFilePathMaybe
        logFunc <- logOptionsHandle stderr False >>= ContT . withLogFunc
        let env = Save.Env
                    { envProfileName = profileName
                    , envConfigName = configName
                    , envProfileFilePath = profileFilePath
                    , envLogFunc = logFunc
                    }
        runRIO env runSave
