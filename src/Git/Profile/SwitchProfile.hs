{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Git.Profile.SwitchProfile where

import qualified Data.Map.Strict        as M
import           Git.Profile.GitProfile
import           Git.Profile.Utils      (throwIfGitUnavailable)
import           RIO
import qualified Turtle

switchProfile :: MonadIO m => GitConfigs -> m ()
switchProfile gitConfigs = Turtle.sh $ do
    throwIfGitUnavailable
    flip traverseWithKey_ gitConfigs $ \configCategory configMap ->
        flip traverseWithKey_ configMap $ \k v ->
            Turtle.proc "git" ["config", "--local", configCategory <> "." <> k, v] Turtle.stdin

traverseWithKey_ :: Applicative f => (k -> a -> f b) -> Map k a -> f ()
traverseWithKey_ = (fmap . fmap) void M.traverseWithKey
