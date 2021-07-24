{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.GitProfile where

import           Control.Monad.IO.Class
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Yaml.Aeson        as Y
import           RIO                    ()
import           Turtle                 ((</>))
import qualified Turtle

type GitProfile = M.Map ProfileName GitConfigs
type ProfileName = T.Text

type GitConfigs = M.Map ConfigCategory (M.Map Key Value)
type ConfigCategory = T.Text
type Key = T.Text
type Value = T.Text

-- |Gets a default path to '.gitprofile'.
defaultGitProfilePath :: MonadIO m => m String
defaultGitProfilePath = do
    h <- liftIO Turtle.home
    pure . Turtle.encodeString $ h </> ".gitprofile"

-- |Loads the specific path as 'GitProfile'.
loadGitProfile :: MonadIO m => String -> m GitProfile
loadGitProfile = Y.decodeFileThrow
