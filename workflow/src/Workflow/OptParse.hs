module Workflow.OptParse
    ( module Workflow.OptParse
    , module Workflow.OptParse.Types
    ) where

import Introduction

import Options.Applicative

import Workflow.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions CommandWaiting Flags Configuration = pure (DispatchWaiting, Settings)
-- Add the option for a config file to give the path to the workflow directory
-- to be used by wf.

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ = ParserPrefs
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
parseCommand = hsubparser $ mconcat
    [ command "waiting" parseCommandWaiting
    ]

parseCommandWaiting :: ParserInfo Command
parseCommandWaiting = info parser modifier
  where
    parser = pure CommandWaiting
    modifier = fullDesc
            <> progDesc "Print a list of the \"waiting\" tasks"

parseFlags :: Parser Flags
parseFlags = pure Flags
