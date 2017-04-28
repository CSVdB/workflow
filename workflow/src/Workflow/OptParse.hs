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
getSettings Flags {..} = Settings shouldprint

getDispatch :: Command -> Flags -> IO Dispatch
getDispatch cmd flags = do
    config <- getConfig flags
    getDispatchFromConfig cmd config

getDispatchFromConfig :: Command -> Configuration -> IO Dispatch
getDispatchFromConfig CommandWaiting Configuration {..} = do
    configPath <- parseAbsDir workDirConfig
    pure (DispatchWaiting configPath)

getConfig :: Flags -> IO Configuration
getConfig Flags {..} = do
    configPath <-
        case configfile of
            Nothing -> defaultConfigFile
            Just path -> resolveFile' path
    (dirPath, shouldPrintConfig) <- extractFromConfigPath configPath
    workDirUsed <-
        case workflowdir of
            Nothing -> pure dirPath
            Just directoryPath -> parseAbsDir directoryPath
    pure $ Configuration (fromAbsDir workDirUsed) shouldPrintConfig

extractFromConfigPath :: Path Abs File -> IO (Path Abs Dir, ShouldPrint)
extractFromConfigPath confPath = do
    config <- load [Optional $ toFilePath confPath]
    dirPath <- lookup config "path"
    workDir <-
        case dirPath of
            Nothing -> do
                home <- getHomeDir
                resolveDir home "workflow"
            Just workDirFound -> parseAbsDir workDirFound
    shouldPrintConfig <- lookup config "shouldPrint"
    case shouldPrintConfig of
        Nothing -> pure (workDir, defaultShouldPrint)
        Just shouldPrint -> pure (workDir, shouldPrint)

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
    home <- getHomeDir
    resolveFile home ".wfrc"

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
parseCommand = hsubparser $ mconcat [command "waiting" parseCommandWaiting]

parseCommandWaiting :: ParserInfo Command
parseCommandWaiting = info parser modifier
  where
    parser = pure CommandWaiting
    modifier = fullDesc <> progDesc "Print a list of the \"waiting\" tasks"

parseFlags :: Parser Flags
parseFlags =
    Flags <$>
    option
        (Just <$> str)
        (mconcat
             [ long "filePath"
             , help "Give the path to an altenative config file"
             , value Nothing
             , metavar "FILEPATH"
             ]) <*>
    option
        (Just <$> str)
        (mconcat
             [ long "config_path"
             , help "Give the path to the workflow directory to be used"
             , value Nothing
             , metavar "FILEPATH"
             ]) <*>
    option
        (maybeReader getShouldPrint)
        (mconcat
             [ long "should_print"
             , help
                   "This describes whether error messages should be handled as errors (\"error\"), warnings (\"warning\") or ignored (\"nothing\")."
             , showDefault
             , value defaultShouldPrint
             , metavar "SHOULDPRINT"
             ])

defaultShouldPrint :: ShouldPrint
defaultShouldPrint = Warning
