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
    cfg <- getConfig cmd flags
    getDispatchFromConfig cmd flags cfg

getDispatchFromConfig :: Command -> Flags -> Configuration -> IO Dispatch
getDispatchFromConfig cmd Flags {..} Configuration {..} =
    let shouldPrint = fromMaybe cfgShouldPrint flagsShouldPrint
    in case cmd of
           CommandWaiting args -> do
               cmdWorkDir <- mapM formatWorkDirPath $ cmdWorkDirPath args
               let workDir = fromMaybe cfgWorkDir cmdWorkDir
               pure $ DispatchWaiting $ WaitingArgsDispatch workDir shouldPrint
           CommandNext args -> do
               projectsGlob <-
                   formatProjectsGlob $
                   fromMaybe cfgProjectsGlob $ cmdProjectsGlob args
               pure $ DispatchNext $ NextArgsDispatch projectsGlob shouldPrint

getConfig :: Command -> Flags -> IO Configuration
getConfig _ flg = do
    cfgPath <- getConfigPathFromFlags flg
    config <- load [Optional $ toFilePath cfgPath]
    workDirPath <- fromMaybe defaultWorkDirPath <$> lookup config "workDir"
    workDir <- formatWorkDirPath workDirPath
    projectsGlob <-
        do unformattedProjectsGlob <-
               fromMaybe (defaultProjectsGlob workDirPath) <$>
               lookup config "projects"
           formatProjectsGlob unformattedProjectsGlob
    shouldPrint <- fromMaybe defaultShouldPrint <$> lookup config "shouldPrint"
    pure $ Configuration workDir projectsGlob shouldPrint

defaultWorkDirPath :: FilePath
defaultWorkDirPath = "~/workflow"

defaultProjectsGlob :: FilePath -> FilePath
defaultProjectsGlob workDir = workDir ++ "/projects"

getConfigPathFromFlags :: Flags -> IO (Path Abs File)
getConfigPathFromFlags Flags {..} =
    fromMaybe defaultConfigFile $ resolveFile' <$> flagsConfigFile

formatWorkDirPath :: String -> IO (Path Abs Dir)
formatWorkDirPath dirPathString =
    case dirPathString of
        '~':'/':_ -> do
            home <- getHomeDir
            resolveDir home $ drop 2 dirPathString
        '~':_ -> do
            home <- getHomeDir
            resolveDir home $ init dirPathString
        '/':_ -> parseAbsDir dirPathString
        _ -> resolveDir' dirPathString

formatProjectsGlob :: String -> IO String
formatProjectsGlob projectsGlob =
    case projectsGlob of
        '~':'/':_ -> do
            home <- getHomeDir
            pure $ fromAbsDir home ++ drop 2 projectsGlob
        '~':_ -> do
            home <- getHomeDir
            pure $ fromAbsDir home ++ init projectsGlob
        '/':_ -> pure projectsGlob
        _ -> do
            currentDir <- getCurrentDir
            pure $ fromAbsDir currentDir ++ projectsGlob

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
    parser = CommandNext . NextArgsCommand <$> projectsGlobParser

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

projectsGlobParser :: Parser (Maybe FilePath)
projectsGlobParser =
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
