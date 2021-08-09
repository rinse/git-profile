{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.Cli.Parser
    ( parseCommandLineOptions
    , commandParserInfo
    , switchArgumentsParserInfo
    , saveArgumentsParserInfo
    ) where

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
commandParser = subparser $ modSwitch <> modSave
    where
    modSwitch = command "switch" $ SwitchCmd <$> switchArgumentsParserInfo
    modSave = command "save" $ SaveCmd <$> saveArgumentsParserInfo

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
switchArgumentsParser = SwitchArguments <$> profileParser <*> profileFilePathParser

saveArgumentsParserInfo :: ParserInfo SaveArguments
saveArgumentsParserInfo = saveArgumentsParser `withInfo` "Saves the current config to the profile"

saveArgumentsParser :: Parser SaveArguments
saveArgumentsParser = SaveArguments <$> profileParser <*> nameParser <*> profileFilePathParser

profileParser :: Parser Text
profileParser = strArgument . execWriter $ do
    tell $ metavar "PROFILE"

nameParser :: Parser Text
nameParser = strArgument . execWriter $ do
    tell $ metavar "NAME"

profileFilePathParser :: Parser (Maybe Text)
profileFilePathParser = optional . strOption . execWriter $ do
    tell $ long "profile-file-path"
    tell $ help "A path to a profile file. The file is a collection of profiles."
    tell . showDefaultWith $ const "$HOME/.gitprofile"
