{-# LANGUAGE MultiParamTypeClasses #-}
module Git.Profile.App.SwitchEnv (Env (..)) where

import qualified Data.Text            as T
import           Git.Profile.HasClass
import           Lens.Micro
import           RIO

data Env = Env
    { envProfileName     :: T.Text
    , envProfileFilePath :: T.Text
    , envLogFunc         :: LogFunc
    }

instance HasProfile Env T.Text where
    profileNameL = lens envProfileName $ \x y -> x { envProfileName = y }

instance HasProfileFilePath Env T.Text where
    profileFilePathL = lens envProfileFilePath $ \x y -> x { envProfileFilePath = y }

instance HasLogFunc Env where
    logFuncL = lens envLogFunc $ \x y -> x { envLogFunc = y }
