{-# LANGUAGE MultiParamTypeClasses #-}
module Git.Profile.App.SaveEnv (Env (..)) where

import qualified Data.Text            as T
import           Git.Profile.HasClass
import           Lens.Micro
import           RIO

data Env = Env
    { envProfileName     :: T.Text
    , envConfigName      :: T.Text
    , envProfileFilePath :: T.Text
    , envLogFunc         :: LogFunc
    }

instance HasProfile Env T.Text where
    profileNameL = lens envProfileName $ \x y -> x { envProfileName = y }

instance HasConfigName Env T.Text where
    configNameL = lens envConfigName $ \x y -> x { envConfigName = y }

instance HasProfileFilePath Env T.Text where
    profileFilePathL = lens envProfileFilePath $ \x y -> x { envProfileFilePath = y }

instance HasLogFunc Env where
    logFuncL = lens envLogFunc $ \x y -> x { envLogFunc = y }
