{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow.OptParse
    ( module Workflow.OptParse
    , module Workflow.OptParse.Types
    ) where

import Data.Configurator
import Data.Configurator.Types
import Import hiding (lookup)
import Options.Applicative
import System.Environment
import Workflow.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    dispatch <- getDispatch cmd flags
    pure (dispatch, getSettings flags)

getSettings :: Flags -> Settings
getSettings _ = Settings

getDispatch :: Command -> Flags -> IO Dispatch
getDispatch cmd flags = do
    cfg <- getConfig cmd flags
    getDispatchFromConfig cmd cfg

getDispatchFromConfig :: Command -> Configuration -> IO Dispatch
getDispatchFromConfig (CommandWaiting _) Configuration {..} =
    pure $ DispatchWaiting $ WaitingArgsDispatch cfgWorkDir cfgShouldPrint
getDispatchFromConfig (CommandNext _) Configuration {..} =
    pure $ DispatchNext $ NextArgsDispatch cfgProjectDir cfgShouldPrint

getConfig :: Command -> Flags -> IO Configuration
getConfig cmd Flags {..} = do
    cfgPath <- getConfigPathFromInput flagsConfigFile
    shouldPrint <- getShouldPrintFromInput cfgPath flagsShouldPrint
    workDirPath <-
        case cmd of
            CommandWaiting args -> getWorkDirFromInput cfgPath $ cmdWorkDir args
            _ -> getDefaultWorkDir cfgPath
    workDir <- parseAbsDir workDirPath
    projectDir <-
        case cmd of
            CommandNext args ->
                getProjectDirFromInput cfgPath $ cmdProjectDir args
            _ -> getDefaultProjectDir cfgPath
    pure $ Configuration workDir projectDir shouldPrint

getConfigPathFromInput :: Maybe FilePath -> IO (Path Abs File)
getConfigPathFromInput configFile =
    fromMaybe defaultConfigFile $ resolveFile' <$> configFile

getShouldPrintFromInput :: Path Abs File -> Maybe ShouldPrint -> IO ShouldPrint
getShouldPrintFromInput configPath shouldPrint = do
    shouldPrintConfig <- getShouldPrintFromConfigPath configPath
    pure $
        fromMaybe (fromMaybe defaultShouldPrint shouldPrintConfig) shouldPrint

getWorkDirFromInput :: Path Abs File -> Maybe FilePath -> IO FilePath
getWorkDirFromInput cfgFile workDir = do
    defaultWorkDir <- getDefaultWorkDir cfgFile
    pure $ fromMaybe defaultWorkDir workDir

getDefaultWorkDir :: Path Abs File -> IO FilePath
getDefaultWorkDir = getSomePathFromConfigPath "workDir"

getProjectDirFromInput :: Path Abs File -> Maybe FilePath -> IO FilePath
getProjectDirFromInput cfgFile projectDir = do
    defaultProjectDir <- getDefaultProjectDir cfgFile
    pure $ fromMaybe defaultProjectDir projectDir

getDefaultProjectDir :: Path Abs File -> IO FilePath
getDefaultProjectDir = getSomePathFromConfigPath "projects"

getSomePathFromConfigPath :: Name -> Path Abs File -> IO FilePath
getSomePathFromConfigPath name confPath = do
    config <- load [Optional $ toFilePath confPath]
    path <- lookup config name
    formatDirPath path

getShouldPrintFromConfigPath :: Path Abs File -> IO (Maybe ShouldPrint)
getShouldPrintFromConfigPath confPath = do
    config <- load [Optional $ toFilePath confPath]
    lookup config "shouldPrint"

formatDirPath :: Maybe String -> IO FilePath
formatDirPath Nothing = do
    home <- getHomeDir
    pure $ fromAbsDir home ++ "workflow"
formatDirPath (Just dirPathString) =
    case dirPathString of
        '~':_ -> do
            home <- getHomeDir
            pure $ fromAbsDir home ++ drop 2 dirPathString
        '/':_ -> pure dirPathString
        _ -> do
            currentDir <- getCurrentDir
            pure $ fromAbsDir currentDir ++ dirPathString

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
    homeDir <- getHomeDir
    resolveFile homeDir ".wfrc"

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Workflow"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser $
    mconcat
        [command "waiting" parseCommandWaiting, command "next" parseCommandNext]

parseCommandWaiting :: ParserInfo Command
parseCommandWaiting = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print a list of the \"waiting\" tasks"
    parser = CommandWaiting . WaitingArgsCommand <$> workflowDirParser

parseCommandNext :: ParserInfo Command
parseCommandNext = info parser modifier
  where
    modifier =
        fullDesc <>
        progDesc
            "Print the next actions and warn when a file does not have one."
    parser = CommandNext . NextArgsCommand <$> projectDirParser

workflowDirParser :: Parser (Maybe FilePath)
workflowDirParser =
    option
        (Just <$> str)
        (mconcat
             [ long "workflow-dir"
             , help "Give the path to the workflow directory to be used"
             , value Nothing
             , metavar "FILEPATH"
             ])

projectDirParser :: Parser (Maybe FilePath)
projectDirParser =
    option
        (Just <$> str)
        (mconcat
             [ long "project-dir"
             , help "Give the path to the project directory to be used"
             , value Nothing
             , metavar "FILEPATH"
             ])

configFileParser :: Parser (Maybe FilePath)
configFileParser =
    option
        (Just <$> str)
        (mconcat
             [ long "config-file"
             , help "Give the path to an altenative config file"
             , value Nothing
             , metavar "FILEPATH"
             ])

shouldPrintParser :: Parser (Maybe ShouldPrint)
shouldPrintParser =
    option
        (Just <$> maybeReader getShouldPrint)
        (mconcat
             [ long "should-print"
             , help
                   "This describes whether error messages should be handled as errors (\"error\"), warnings (\"warning\") or ignored (\"nothing\")."
             , showDefault
             , value Nothing
             , metavar "shouldPrint"
             ])

parseFlags :: Parser Flags
parseFlags = Flags <$> configFileParser <*> shouldPrintParser
