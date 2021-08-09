{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.GitProfile
    ( GitProfile
    , ProfileName
    , GitConfigs
    , ConfigCategory
    , envOrDefaultGitProfilePath
    , loadGitProfile
    , writeGitProfile
    , getGitConfigs
    ) where

import           Control.Exception.Safe as E
import           Control.Monad.IO.Class
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import qualified Data.Yaml.Aeson        as Y
import           RIO                    ()
import           System.Environment     (lookupEnv)
import           Turtle                 ((</>))
import qualified Turtle

type GitProfile = M.Map ProfileName GitConfigs
type ProfileName = T.Text

type GitConfigs = M.Map ConfigCategory (M.Map T.Text T.Text)
type ConfigCategory = T.Text

-- |Gets a default path to '.gitprofile'.
defaultGitProfilePath :: MonadIO m => m FilePath
defaultGitProfilePath = do
    h <- liftIO Turtle.home
    pure . Turtle.encodeString $ h </> ".gitprofile"

-- |Gets a path to the git profile from environment variables.
envGitProfilePath :: MonadIO m => m (Maybe FilePath)
envGitProfilePath = liftIO $ lookupEnv "GIT_PROFILE_PATH"

-- |Gets a path to the git profile from environment variables if possible,
--  gets a default path otherwise.
envOrDefaultGitProfilePath :: MonadIO m => m FilePath
envOrDefaultGitProfilePath = envGitProfilePath >>= maybe defaultGitProfilePath pure

-- |Loads the specific path as 'GitProfile'.
loadGitProfile :: MonadIO m => FilePath -> m GitProfile
loadGitProfile = Y.decodeFileThrow

-- |Writes .gitprofile
writeGitProfile :: MonadIO m => FilePath -> GitProfile -> m ()
writeGitProfile = (fmap . fmap) liftIO Y.encodeFile

-- |Gets git configs from GitProfile.
getGitConfigs :: E.MonadThrow m => T.Text -> GitProfile -> m GitConfigs
getGitConfigs profileName profile =
    case M.lookup profileName profile of
        Nothing -> E.throwString $ "Profile not found: " <> T.unpack profileName
        Just a  -> pure a
