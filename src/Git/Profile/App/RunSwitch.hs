{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.App.RunSwitch (runSwitch) where

import           Control.Monad.Reader.Class
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
