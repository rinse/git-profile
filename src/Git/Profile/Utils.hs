{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.Utils where

import           RIO
import qualified Turtle

throwIfGitUnavailable :: MonadIO m => m ()
throwIfGitUnavailable = do
    exitCode <- Turtle.proc "git" ["rev-parse"] Turtle.stdin
    unless (exitCode == ExitSuccess) $
        RIO.throwString "The current directory is not under git control."
