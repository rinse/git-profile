{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.SaveProfile (readConfigName, saveProfile) where

import           Control.Monad
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import           Git.Profile.GitProfile
import           Git.Profile.HasClass
import           Git.Profile.Utils
import           RIO
import qualified Turtle

saveProfile :: (MonadIO m, MonadReader env m, HasProfileFilePath env Text)
            => Text -- ^A profile name which is associated the configs.
            -> Text -- ^A config name which is a key for git-config.
            -> m ()
saveProfile profileName configName = do
    profileFilePath <- view profileFilePathL
    Turtle.sh $ do
        throwIfGitUnavailable
        profile <- loadGitProfile . T.unpack $ profileFilePath
        configValue <- Turtle.lineToText <$> Turtle.inproc "git" ["config", "--get", configName] Turtle.stdin
        (section, key) <- case readConfigName configName of
            Just pair -> pure pair
            Nothing -> throwString "configName should be composed of a section and a key."
        let configValueMap = M.fromList [(section, M.fromList [(key, configValue)])]
            newProfile = M.insertWith (flip (<>)) profileName configValueMap profile
        writeGitProfile (T.unpack profileFilePath) newProfile

-- |Splits a name which is composed like `Section.Key` into `Section` and `Key`.
readConfigName :: Text -> Maybe (Text, Text)
readConfigName configName = do
    let (h, b) = T.break (== '.') configName
    (_, b') <- T.uncons b
    guard . not $ T.null h
    guard . not $ T.null b'
    pure (h, b')
