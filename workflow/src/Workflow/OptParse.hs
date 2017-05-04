{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow.OptParse
    ( module Workflow.OptParse
    , module Workflow.OptParse.Types
    ) where

import Data.Configurator
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
    config <- getConfig cmd flags
    getDispatchFromConfig config

getDispatchFromConfig :: Configuration -> IO Dispatch
getDispatchFromConfig ConfigWaiting {..} = do
    configPath <- parseAbsDir workDirConfig
    pure $ DispatchWaiting configPath shouldPrintConfig
getDispatchFromConfig ConfigNext {..} = do
    configPath <- parseAbsDir projectDirConfig
    pure $ DispatchNext configPath shouldPrintConfig

getConfig :: Command -> Flags -> IO Configuration
getConfig CommandWaiting {..} _ = do
    configPath <- getConfigPathFromInput configFile
    shouldPrint <- getShouldPrintFromInput configPath shouldPrintWaiting
    workDir <-
        fromAbsDir <$>
        case workDirCommand of
            Nothing -> getWorkDirPathFromConfigPath configPath
            Just directoryPath -> parseAbsDir directoryPath
    pure $ ConfigWaiting workDir shouldPrint
getConfig CommandNext {..} _ = do
    configPath <- getConfigPathFromInput configFile
    shouldPrint <- getShouldPrintFromInput configPath shouldPrintNext
    projectDir <-
        fromAbsDir <$>
        case projectDirCommand of
            Nothing -> getProjectDirPathFromConfigPath configPath
            Just directoryPath -> parseAbsDir directoryPath
    pure $ ConfigNext projectDir shouldPrint

getConfigPathFromInput :: Maybe FilePath -> IO (Path Abs File)
getConfigPathFromInput configFile =
    case configFile of
        Nothing -> defaultConfigFile
        Just path -> resolveFile' path

getShouldPrintFromInput :: Path Abs File -> Maybe ShouldPrint -> IO ShouldPrint
getShouldPrintFromInput configPath shouldPrint =
    case shouldPrint of
        Just x -> pure x
        Nothing -> do
            shouldPrintConfig <-
                do config <- load [Optional $ toFilePath configPath]
                   lookup config "shouldPrint"
            case shouldPrintConfig of
                Nothing -> pure defaultShouldPrint
                Just x -> pure x

getWorkDirPathFromConfigPath :: Path Abs File -> IO (Path Abs Dir)
getWorkDirPathFromConfigPath confPath = do
    config <- load [Optional $ toFilePath confPath]
    dirPathString <- lookup config "workDir"
    formatDirPath dirPathString

getProjectDirPathFromConfigPath :: Path Abs File -> IO (Path Abs Dir)
getProjectDirPathFromConfigPath confPath = do
    config <- load [Optional $ toFilePath confPath]
    dirPathString <- lookup config "projectsDir"
    formatDirPath dirPathString

formatDirPath :: Maybe String -> IO (Path Abs Dir)
formatDirPath Nothing = do
    home <- getHomeDir
    resolveDir home "workflow"
formatDirPath (Just dirPathString) =
    case dirPathString of
        '~':_ -> do
            home <- getHomeDir
            dirPath <- resolveDir home $ drop 2 dirPathString
            print dirPath
            pure dirPath
        _ -> do
            currentDir <- getCurrentDir
            resolveDir currentDir dirPathString

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
    parser =
        CommandWaiting <$> workflowDirParser <*> configFileParser <*>
        shouldPrintParser

parseCommandNext :: ParserInfo Command
parseCommandNext = info parser modifier
  where
    modifier =
        fullDesc <>
        progDesc
            "Print the list of \"next\" tasks as well as the files which don't have a \"next\" action"
    parser =
        CommandNext <$> projectDirParser <*> configFileParser <*>
        shouldPrintParser

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
parseFlags = pure Flags
