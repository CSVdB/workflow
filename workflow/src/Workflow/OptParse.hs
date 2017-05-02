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
    getDispatchFromConfig cmd config

getDispatchFromConfig :: Command -> Configuration -> IO Dispatch
getDispatchFromConfig CommandWaiting {..} Configuration {..} = do
    configPath <- parseAbsDir workDirConfig
    pure $ DispatchWaiting configPath shouldPrintConfig
getDispatchFromConfig CommandNext {..} Configuration {..} = do
    configPath <- parseAbsDir workDirConfig
    pure $ DispatchNext configPath shouldPrintConfig

getConfig :: Command -> Flags -> IO Configuration
getConfig CommandWaiting {..} _ =
    getConfigFromInput workDirCommand configFile shouldPrint
getConfig CommandNext {..} _ =
    getConfigFromInput workDirCommand configFile shouldPrint

getConfigFromInput :: Maybe FilePath
                   -> Maybe FilePath
                   -> ShouldPrint
                   -> IO Configuration
getConfigFromInput workDir configFile _ = do
    configPath <-
        case configFile of
            Nothing -> defaultConfigFile
            Just path -> resolveFile' path
    dirPath <- getDirPathFromConfigPath configPath
    shouldPrintConfig <- getShouldPrintPathFromConfigPath configPath
    workDirConfig <-
        case workDir of
            Nothing -> pure dirPath
            Just directoryPath -> parseAbsDir directoryPath
    pure $ Configuration (fromAbsDir workDirConfig) shouldPrintConfig

getDirPathFromConfigPath :: Path Abs File -> IO (Path Abs Dir)
getDirPathFromConfigPath confPath = do
    config <- load [Optional $ toFilePath confPath]
    dirPathString <- lookup config "path"
    formatDirPath dirPathString

getShouldPrintPathFromConfigPath :: Path Abs File -> IO ShouldPrint
getShouldPrintPathFromConfigPath confPath = do
    config <- load [Optional $ toFilePath confPath]
    shouldPrintConfig <- lookup config "shouldPrint"
    case shouldPrintConfig of
        Nothing -> pure defaultShouldPrint
        Just shouldPrint -> pure shouldPrint

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
parseCommandWaiting = info commandParser modifier
  where
    modifier = fullDesc <> progDesc "Print a list of the \"waiting\" tasks"

parseCommandNext :: ParserInfo Command
parseCommandNext = info commandParser modifier
  where
    modifier =
        fullDesc <>
        progDesc
            "Print the list of \"next\" tasks as well as the files which don't have a \"next\" action"

commandParser :: Parser Command
commandParser =
    CommandWaiting <$>
    option
        (Just <$> str)
        (mconcat
             [ long "config-file"
             , help "Give the path to an altenative config file"
             , value Nothing
             , metavar "FILEPATH"
             ]) <*>
    option
        (Just <$> str)
        (mconcat
             [ long "workflow-dir"
             , help "Give the path to the workflow directory to be used"
             , value Nothing
             , metavar "FILEPATH"
             ]) <*>
    option
        (maybeReader getShouldPrint)
        (mconcat
             [ long "should-print"
             , help
                   "This describes whether error messages should be handled as errors (\"error\"), warnings (\"warning\") or ignored (\"nothing\")."
             , showDefault
             , value defaultShouldPrint
             , metavar "shouldPrint"
             ])

parseFlags :: Parser Flags
parseFlags = pure Flags
