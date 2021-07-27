module Git.Profile.Cli.CommandLineOptions (CommandLineOptions (..), SwitchArguments (..)) where

import qualified Data.Text as T
import           RIO

newtype CommandLineOptions
    = SwitchCmd SwitchArguments
    deriving (Read, Show, Eq, Ord)

data SwitchArguments = SwitchArguments
    { switchArgumentsProfile         :: T.Text
    , switchArgumentsProfileFilePath :: Maybe T.Text
    } deriving (Read, Show, Eq, Ord)
