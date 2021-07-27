{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.Cli.Parser (parseCommandLineOptions, commandParserInfo, switchArgumentsParserInfo) where

import           Control.Applicative
import           Control.Monad.Writer.Strict
import           Git.Profile.Cli.CommandLineOptions
import           Options.Applicative
import           RIO


parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = customExecParser (prefs showHelpOnEmpty) commandParserInfo

withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (helper <*> p) . progDesc

{- |Parses arguments in pure contexts.
>>> execParserPure defaultPrefs commandParserInfo ["switch", "sample"]
Success (SwitchCmd (SwitchArguments {profile = "sample"}))
-}
commandParserInfo :: ParserInfo CommandLineOptions
commandParserInfo = commandParser `withInfo` "Commits a command. See each commands for details."

commandParser :: Parser CommandLineOptions
commandParser = subparser modSwitch
    where
    modSwitch = command "switch" $ SwitchCmd <$> switchArgumentsParserInfo

{- |Parses arguments in pure contexts.
>>> execParserPure defaultPrefs switchArgumentsParserInfo ["profile"]
Success (SwitchArguments {profile = "profile"})

>>> execParserPure defaultPrefs switchArgumentsParserInfo []
Failure (ParserFailure (Missing: PROFILE
<BLANKLINE>
Usage: <program> PROFILE
  Switches a git profile.,ExitFailure 1,80))
-}
switchArgumentsParserInfo :: ParserInfo SwitchArguments
switchArgumentsParserInfo = switchArgumentsParser `withInfo` "Switches a git profile."

switchArgumentsParser :: Parser SwitchArguments
switchArgumentsParser = do
    profile <- strArgument . execWriter $ do
        tell $ metavar "PROFILE"
    profileFilePath <- optional . strOption . execWriter $ do
        tell $ long "profile-file-path"
        tell $ help "A path to a profile file. The file is a collection of profiles."
        tell . showDefaultWith $ const "$HOME/.gitprofile"
    pure $ SwitchArguments profile profileFilePath
