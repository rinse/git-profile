module Git.Profile.App.RunSave (runSave) where

import           Git.Profile.App.SaveEnv
import           Git.Profile.HasClass
import           Git.Profile.SaveProfile
import           RIO

-- git profile save rinse user.name
runSave :: RIO Env ()
runSave = do
    profileName <- view profileNameL
    configName <- view configNameL
    saveProfile profileName configName
