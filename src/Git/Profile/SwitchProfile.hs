{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Git.Profile.SwitchProfile where

import           Control.Exception.Safe as E
import qualified Data.Map               as M
import           Git.Profile.GitProfile (GitConfigs)
import           RIO
import qualified Turtle

switchProfile :: MonadIO m => GitConfigs -> m ()
switchProfile gitConfigs = Turtle.sh $ do
    exitCode <- Turtle.proc "git" ["rev-parse"] Turtle.stdin
    unless (exitCode == ExitSuccess) $
        E.throwString "The current directory is not under git control."
    flip traverseWithKey_ gitConfigs $ \configCategory configMap ->
        flip traverseWithKey_ configMap $ \k v ->
            Turtle.proc "git" ["config", "--local", configCategory <> "." <> k, v] Turtle.stdin

traverseWithKey_ :: Applicative f => (k -> a -> f b) -> Map k a -> f ()
traverseWithKey_ = (fmap . fmap) void M.traverseWithKey
