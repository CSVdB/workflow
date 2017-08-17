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
                       fromMaybe defaultWorkDirPath $
                       mplus (cmdWorkDirPath args) cfgWorkDir
               DispatchWaiting . (`WaitingArgsDispatch` shouldPrint) <$>
                   formatWorkDirPath workDirPath
           CommandNext args -> do
               (projectDir, projectFiles) <-
                   getProjectFilesFromProjectsGlob $
                   fromMaybe (defaultProjectsGlob defaultWorkDirPath) $
                   mplus (cmdProjectsGlob args) cfgProjectsGlob
               pure $
                   DispatchNext $
                   NextArgsDispatch projectDir projectFiles shouldPrint
           CommandRem RemArgsCommand {..} -> do
               let workDirPath =
                       fromMaybe defaultWorkDirPath $
                       mplus (cmdWorkDirPath cmdWaitArgs) cfgWorkDir
               workDir <- formatWorkDirPath workDirPath
               let maxDays =
                       fromMaybe defaultMaxDays $ mplus cmdMaxDays cfgMaxDays
               fromEmailAddress <-
                   case cmdFromAddress of
                       Just text -> pure text
                       Nothing ->
                           case cfgFromAddress of
                               Just text -> pure text
                               Nothing ->
                                   die $
                                   unlines
                                       [ "No email address was given to send emails from."
                                       , "Add it to the config file by using "
                                       , "\"fromAddress = user@example.com\" and "
                                       , "\"name = user\"."
                                       ]
               let fromName =
                       T.pack <$> mplus cmdMailSenderName cfgMailSenderName
               let templateMaybe = mplus cmdTemplateFile cfgTemplateFile
               template <-
                   case templateMaybe of
                       Nothing ->
                           die
                               "No global template was given. Check the README to see how to set up reminders."
                       Just list -> pure list
               mailTemplate <-
                   templateToMailTemplate <$> resolveStringToFiles template
               case mailTemplate of
                   Left errMess -> die errMess
                   Right temp ->
                       pure $
                       DispatchRem $
                       RemArgsDispatch
                           (WaitingArgsDispatch workDir shouldPrint)
                           maxDays
                           (Address fromName fromEmailAddress)
                           temp

templateToMailTemplate :: [Path Abs File] -> Either String MailTemplate
templateToMailTemplate files =
    let headerFileEither =
            case getFileWithExtension ".header" files of
                Nothing ->
                    Left "The global template has no or multiple header files."
                Just file -> Right file
        plainFileMaybe = getFileWithExtension ".txt" files
        htmlFileMaybe = getFileWithExtension ".html" files
        filesWithExtension =
            case (plainFileMaybe, htmlFileMaybe) of
                (Nothing, Nothing) ->
                    Left
                        "The global template contains either no or multiple html and plain files."
                (Just file, Nothing) -> Right (file, Nothing)
                (Nothing, Just file) -> Right (file, Nothing)
                (Just plainFile, Just htmlFile) ->
                    Right (plainFile, Just htmlFile)
    in case (,) <$> headerFileEither <*> filesWithExtension of
           Left errMess -> Left errMess
           Right (headerFile, (file1, file2)) ->
               Right $ MailTemplate headerFile file1 file2

getFileWithExtension :: Text -> [Path Abs File] -> Maybe (Path Abs File)
getFileWithExtension ext = find ((==) ext . T.pack . fileExtension)

resolveStringToFiles :: FilePath -> IO [Path Abs File]
resolveStringToFiles path = do
    templatesDir <- getTemplatesDir
    files <-
        mapM (resolveFile templatesDir) $
        addExtension path <$> [".header", ".txt", ".html"]
    filterM doesFileExist files

getTemplatesDir :: IO (Path Abs Dir)
getTemplatesDir = do
    homeDir <- getHomeDir
    resolveDir homeDir ".wf/reminder-templates/"

getConfig :: Flags -> IO Configuration
getConfig flg = do
    cfgPath <- getConfigPathFromFlags flg
    config <- load [Optional $ toFilePath cfgPath]
    Configuration <$> lookup config "workDir" <*> lookup config "projects" <*>
        lookup config "shouldPrint" <*>
        lookup config "maxDays" <*>
        lookup config "fromAddress" <*>
        lookup config "name" <*>
        lookup config "templateFile"

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
            (,) projectDir <$> getOrgFilesFromDirRecur projectDir
        '*':reverseProjectGlob -> do
            projectDir <- parseAbsDir $ reverse reverseProjectGlob
            (,) projectDir <$> getOrgFilesFromDir projectDir
        _ -> do
            projectDir <- parseAbsDir projectsGlobFormattedStart
            (,) projectDir <$> getOrgFilesFromDir projectDir

formatBeginningProjectsGlob :: String -> IO String
formatBeginningProjectsGlob projectsGlob =
    case projectsGlob of
        '~':'/':endOfProjectsGlob -> do
            home <- getHomeDir
            pure $ fromAbsDir home ++ endOfProjectsGlob
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
         mailSenderNameParser <*>
         templateFileParser)

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
             , metavar "GLOB"
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

templateFileParser :: Parser (Maybe String)
templateFileParser =
    option
        (Just <$> str)
        (mconcat
             [ long "template-mail-file"
             , help
                   "Give \"file\" to use the global mail template in \"/home/user/.wf/reminder-templates/file.(header/txt/html)\" or the absolute path without extension."
             , value Nothing
             , metavar "STRING"
             ])

parseFlags :: Parser Flags
parseFlags = Flags <$> configFileParser <*> shouldPrintParser
