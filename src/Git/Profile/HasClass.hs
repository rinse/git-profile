{-# LANGUAGE MultiParamTypeClasses #-}
module Git.Profile.HasClass where

import           Lens.Micro

class HasProfile s a where
    profileNameL :: Lens' s a

class HasProfileFilePath s a where
    profileFilePathL :: Lens' s a
