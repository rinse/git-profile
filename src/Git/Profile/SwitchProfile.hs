{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Git.Profile.SwitchProfile where

import           Control.Exception.Safe (throwString)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map               as M
import           Git.Profile.GitProfile
import           Turtle


switchProfile :: MonadIO m => GitConfigs -> m ()
switchProfile config = sh $ do
    exitCode <- proc "git" ["rev-parse"] stdin
    unless (exitCode == ExitSuccess) $
        throwString "The current directory is not under git control."
    void . flip M.traverseWithKey config $ \category m ->
        flip M.traverseWithKey m $ \k v ->
            proc "git" ["config", "--local", category <> "." <> k, v] stdin
