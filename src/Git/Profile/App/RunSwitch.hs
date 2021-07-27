{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.App.RunSwitch (runSwitch) where

import           Control.Exception.Safe     as E
import           Control.Monad.Reader.Class
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import           Git.Profile.App.SwitchEnv
import           Git.Profile.GitProfile
import           Git.Profile.HasClass
import           Git.Profile.SwitchProfile
import           Lens.Micro
import           RIO

runSwitch :: RIO Env ()
runSwitch = do
    env <- ask
    let profileName = env ^. profileNameL
    profile <- loadGitProfile . T.unpack $ env ^. profileFilePathL
    configs <- getGitConfigs profileName profile
    switchProfile configs
    logInfo $ "GitProfile is switched to " <> display profileName

getGitConfigs :: E.MonadThrow m => T.Text -> GitProfile -> m GitConfigs
getGitConfigs profileName profile =
    case M.lookup profileName profile of
        Nothing -> E.throwString $ "Profile not found: " <> T.unpack profileName
        Just a  -> pure a
