{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.Command.Parser where

import           Control.Monad.Writer.Strict
import           Data.List
import qualified Data.Map.Strict             as M
import qualified Data.Text                   as T
import           Git.Profile.Command.Command
import           Git.Profile.GitProfile      (gitProfile)
import           Options.Applicative


parseCommand :: IO Command
parseCommand = execParser commandParserInfo

withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (helper <*> p) . progDesc

{- |Parses arguments in pure contexts.
>>> execParserPure defaultPrefs commandParserInfo ["switch", "sample"]
Success (SwitchCmd (SwitchArguments {profile = "sample"}))
-}
commandParserInfo :: ParserInfo Command
commandParserInfo = commandParser `withInfo` "Commits a command. See each commands for details."

commandParser :: Parser Command
commandParser = subparser modSwitch
    where
    modSwitch = command "switch" $ SwitchCmd <$> switchArgumentsParserInfo

{- |Parses arguments in pure contexts.
>>> execParserPure defaultPrefs switchArgumentsParserInfo ["profile"]
Success (SwitchArguments {profile = "profile"})

>>> execParserPure defaultPrefs switchArgumentsParserInfo []
Failure (ParserFailure (Missing: PROFILE
<BLANKLINE>
Usage: <program> PROFILE [--path PROFILE_PATH]
  Switches a git profile.,ExitFailure 1,80))
-}
switchArgumentsParserInfo :: ParserInfo SwitchArguments
switchArgumentsParserInfo = switchArgumentsParser `withInfo` "Switches a git profile."

switchArgumentsParser :: Parser SwitchArguments
switchArgumentsParser = SwitchArguments
    <$> strArgument (execWriter $ do
            tell $ metavar "PROFILE"
            tell . completer . mkCompleter $ \arg ->
                filter (arg `isPrefixOf`) . fmap T.unpack . M.keys <$> gitProfile
        )
