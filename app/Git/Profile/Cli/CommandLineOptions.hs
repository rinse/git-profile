module Git.Profile.Cli.CommandLineOptions (CommandLineOptions (..), SwitchArguments (..), SaveArguments (..)) where

import qualified Data.Text as T
import           RIO

data CommandLineOptions
    = SwitchCmd SwitchArguments
    | SaveCmd   SaveArguments
    deriving (Read, Show, Eq, Ord)

data SwitchArguments = SwitchArguments
    { switchArgumentsProfile         :: T.Text
    , switchArgumentsProfileFilePath :: Maybe T.Text
    } deriving (Read, Show, Eq, Ord)

data SaveArguments = SaveArguments
    { saveArgumentsProfile         :: T.Text
    , saveArgumentsName            :: T.Text
    , saveArgumentsProfileFilePath :: Maybe T.Text
    } deriving (Read, Show, Eq, Ord)
