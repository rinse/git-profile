{-# LANGUAGE RecordWildCards #-}

module Git.Profile.Command.Command where

import           Control.Exception.Safe
import           Control.Monad.IO.Class
import qualified Data.Map.Strict           as M
import qualified Data.Text                 as T
import           Git.Profile.GitProfile    (gitProfile)
import           Git.Profile.SwitchProfile


newtype Command
    = SwitchCmd SwitchArguments
    deriving (Read, Show)

newtype SwitchArguments = SwitchArguments
    { profile     :: T.Text
    } deriving (Read, Show)

runCommand :: (MonadIO m, MonadThrow m) => Command -> m ()
runCommand (SwitchCmd SwitchArguments {..}) = do
    p <- gitProfile
    config <- case M.lookup profile p of
        Nothing -> throwString $ "Profile not found: " <> T.unpack profile
        Just a' -> return a'
    switchProfile config
