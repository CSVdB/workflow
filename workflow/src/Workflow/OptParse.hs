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
    config <- getConfig flags
    getDispatchFromConfig cmd config

getDispatchFromConfig :: Command -> Configuration -> IO Dispatch
getDispatchFromConfig CommandWaiting Configuration {..} = do
    configPath <- parseAbsDir confDataDir
    pure (DispatchWaiting configPath)

getConfig :: Flags -> IO Configuration
getConfig flags = do
    configPath <- getWorkPath flags
    pure Configuration {confDataDir = fromAbsDir configPath}

getWorkPath :: Flags -> IO (Path Abs Dir)
getWorkPath flags@Flags {..} =
    case confPath of
        Nothing -> do
            configPath <- getConfigPath flags
            getWorkPathFromConfigPath configPath
        Just configPath -> parseAbsDir configPath

getConfigPath :: Flags -> IO (Path Abs File)
getConfigPath Flags {..} =
    case workPath of
        Nothing -> defaultConfigFile
        Just path -> resolveFile' path

getWorkPathFromConfigPath :: Path Abs File -> IO (Path Abs Dir)
getWorkPathFromConfigPath confPath = do
    config <- load [Optional $ toFilePath confPath] :: IO Config
    configPath <- lookup config "path"
    case configPath of
        Nothing -> do
            home <- getHomeDir
            resolveDir home "workflow"
        Just directoryPath -> parseAbsDir directoryPath

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
             ])
