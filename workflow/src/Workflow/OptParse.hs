{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow.OptParse
    ( module Workflow.OptParse
    , module Workflow.OptParse.Types
    ) where

import Data.Configurator
import Data.Text (Text)
import qualified Data.Text as T
import Import hiding (lookup)
import Network.CGI.Protocol
import Network.Mail.Mime
import Options.Applicative
import System.Environment
import System.FilePath.Posix
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
    cfg <- getConfig flags
    getDispatchFromConfig cmd flags cfg

getDispatchFromConfig :: Command -> Flags -> Configuration -> IO Dispatch
getDispatchFromConfig cmd Flags {..} Configuration {..} =
    let shouldPrint =
            fromMaybe
                (fromMaybe defaultShouldPrint cfgShouldPrint)
                flagsShouldPrint
    in case cmd of
           CommandWaiting args -> do
               let workDirPath =
                       fromMaybe (fromMaybe defaultWorkDirPath cfgWorkDir) $
                       cmdWorkDirPath args
               workDir <- formatWorkDirPath workDirPath
               pure $ DispatchWaiting $ WaitingArgsDispatch workDir shouldPrint
           CommandNext args -> do
               (projectDir, projectFiles) <-
                   getProjectFilesFromProjectsGlob $
                   fromMaybe
                       (fromMaybe
                            (defaultProjectsGlob defaultWorkDirPath)
                            cfgProjectsGlob) $
                   cmdProjectsGlob args
               pure $
                   DispatchNext $
                   NextArgsDispatch projectDir projectFiles shouldPrint
           CommandRem RemArgsCommand {..} -> do
               let workDirPath =
                       fromMaybe (fromMaybe defaultWorkDirPath cfgWorkDir) $
                       cmdWorkDirPath cmdWaitArgs
               workDir <- formatWorkDirPath workDirPath
               let maxDays =
                       fromMaybe
                           (fromMaybe defaultMaxDays cfgMaxDays)
                           cmdMaxDays
               fromEmailAddress <-
                   case cmdFromAddress of
                       Just text -> pure text
                       Nothing ->
                           case cfgFromAddress of
                               Just text -> pure text
                               Nothing -> die "No email address was given!"
               let fromName =
                       T.pack <$> mplus cmdMailSenderName cfgMailSenderName
               let fromAddress = Address fromName fromEmailAddress
               pure $
                   DispatchRem $
                   RemArgsDispatch
                       (WaitingArgsDispatch workDir shouldPrint)
                       maxDays
                       fromAddress

getConfig :: Flags -> IO Configuration
getConfig flg = do
    cfgPath <- getConfigPathFromFlags flg
    config <- load [Optional $ toFilePath cfgPath]
    workDir <- lookup config "workDir"
    projectsGlob <- lookup config "projects"
    shouldPrint <- lookup config "shouldPrint"
    maxDays <- lookup config "maxDays"
    fromAddress <- lookup config "fromAddress"
    name <- lookup config "name"
    pure $
        Configuration workDir projectsGlob shouldPrint maxDays fromAddress name

defaultWorkDirPath :: FilePath
defaultWorkDirPath = "~/workflow"

defaultProjectsGlob :: FilePath -> FilePath
defaultProjectsGlob workDir = workDir ++ "/projects/*"

defaultMaxDays :: Int
defaultMaxDays = 7

getConfigPathFromFlags :: Flags -> IO (Path Abs File)
getConfigPathFromFlags Flags {..} =
    fromMaybe defaultConfigFile $ resolveFile' <$> flagsConfigFile

formatWorkDirPath :: String -> IO (Path Abs Dir)
formatWorkDirPath dirPathString =
    case dirPathString of
        '~':'/':_ -> do
            home <- getHomeDir
            resolveDir home $ drop 2 dirPathString
        _ -> resolveDir' dirPathString

getProjectFilesFromProjectsGlob :: String -> IO (Path Abs Dir, [Path Abs File])
getProjectFilesFromProjectsGlob projectsGlob = do
    projectsGlobFormattedStart <- formatBeginningProjectsGlob projectsGlob
    case reverse projectsGlobFormattedStart of
        '*':'*':reverseProjectGlob -> do
            projectDir <- parseAbsDir $ reverse reverseProjectGlob
            files <- getOrgFilesFromDirRecur projectDir
            pure (projectDir, files)
        '*':reverseProjectGlob -> do
            projectDir <- parseAbsDir $ reverse reverseProjectGlob
            files <- getOrgFilesFromDir projectDir
            pure (projectDir, files)
        _ -> do
            projectDir <- parseAbsDir projectsGlobFormattedStart
            files <- getOrgFilesFromDir projectDir
            pure (projectDir, files)

formatBeginningProjectsGlob :: String -> IO String
formatBeginningProjectsGlob projectsGlob =
    case projectsGlob of
        '~':'/':_ -> do
            home <- getHomeDir
            pure $ fromAbsDir home ++ drop 2 projectsGlob
        '/':_ -> pure projectsGlob
        _ -> do
            currentDir <- getCurrentDir
            pure $ fromAbsDir currentDir ++ projectsGlob

getOrgFilesFromDir :: Path Abs Dir -> IO [Path Abs File]
getOrgFilesFromDir projectDir = filter isOrgFile . snd <$> listDir projectDir

getOrgFilesFromDirRecur :: Path Abs Dir -> IO [Path Abs File]
getOrgFilesFromDirRecur projectDir =
    filter isOrgFile . snd <$> listDirRecur projectDir

isOrgFile :: Path Abs File -> Bool
isOrgFile file = (not . isHidden) file && fileExtension file == ".org"

isHidden :: Path Abs File -> Bool
isHidden = any startsWithDot . splitDirectories . fromAbsFile

startsWithDot :: FilePath -> Bool
startsWithDot ('.':_) = True
startsWithDot _ = False

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
        [ command "waiting" parseCommandWaiting
        , command "next" parseCommandNext
        , command "reminders" parseCommandRem
        ]

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

parseCommandRem :: ParserInfo Command
parseCommandRem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print a list of the \"waiting\" tasks"
    parser =
        CommandRem <$>
        (RemArgsCommand <$> (WaitingArgsCommand <$> workflowDirParser) <*>
         maxDaysParser <*>
         fromAddressParser <*>
         mailSenderNameParser)

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

projectsGlobParser :: Parser (Maybe String)
projectsGlobParser =
    option
        (Just <$> str)
        (mconcat
             [ long "project-glob"
             , help $
               "Give the path to the project directory to be used" ++
               " plus a ** or * determining whether orgfiles should be extracted recursively resp. directly"
             , value Nothing
             , metavar "STRING"
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

maxDaysParser :: Parser (Maybe Int)
maxDaysParser =
    option
        (Just <$> maybeReader maybeRead)
        (mconcat
             [ long "max-days"
             , help "The max # days of waiting before a reminder can be sent."
             , showDefault
             , value Nothing
             , metavar "INT"
             ])

fromAddressParser :: Parser (Maybe Text)
fromAddressParser =
    option
        (Just <$> maybeReader maybeRead)
        (mconcat
             [ long "from-address"
             , help "The address from which the email will be sent."
             , showDefault
             , value Nothing
             , metavar "STRING"
             ])

mailSenderNameParser :: Parser (Maybe String)
mailSenderNameParser =
    option
        (Just <$> str)
        (mconcat
             [ long "mail-sender-name"
             , help "Full name of the sender of emails"
             , value Nothing
             , metavar "STRING"
             ])

parseFlags :: Parser Flags
parseFlags = Flags <$> configFileParser <*> shouldPrintParser
