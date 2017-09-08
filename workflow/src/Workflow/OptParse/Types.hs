{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workflow.OptParse.Types where

import Data.Text (Text)
import Import
import Network.Mail.Mime
import Workflow.Types

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype CommandWaitingArgs = CommandWaitingArgs
    { cmdWorkDirPath :: Maybe FilePath
    } deriving (Show, Eq)

newtype CommandNextArgs = CommandNextArgs
    { cmdProjectsGlob :: Maybe String
    } deriving (Show, Eq)

data CommandRemArgs = CommandRemArgs
    { cmdWaitArgs :: CommandWaitingArgs
    , cmdMaxDays :: Maybe Int
    , cmdFromAddress :: Maybe Text
    , cmdMailSenderName :: Maybe String
    , cmdTemplateFile :: Maybe FilePath
    } deriving (Show, Eq)

data Command
    = CommandWaiting CommandWaitingArgs
    | CommandNext CommandNextArgs
    | CommandRem CommandRemArgs
    deriving (Show, Eq)

data Flags = Flags
    { flagsConfigFile :: Maybe FilePath
    , flagsShouldPrint :: Maybe ShouldPrint
    } deriving (Show, Eq)

data Configuration = Configuration
    { cfgWorkDir :: Maybe FilePath
    , cfgProjectsGlob :: Maybe String
    , cfgShouldPrint :: Maybe ShouldPrint
    , cfgMaxDays :: Maybe Int
    , cfgFromAddress :: Maybe Text
    , cfgMailSenderName :: Maybe String
    , cfgTemplateFile :: Maybe FilePath
    } deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

defaultShouldPrint :: ShouldPrint
defaultShouldPrint = Warning

data DispatchWaitingArgs = DispatchWaitingArgs
    { dspWorkDir :: Path Abs Dir
    , dspWaitingShouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data DispatchNextArgs = DispatchNextArgs
    { dspProjectDir :: Path Abs Dir
    , dspProjectFiles :: [Path Abs File]
    , dspNextShouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data DispatchRemArgs = DispatchRemArgs
    { dspWaitArgs :: DispatchWaitingArgs
    , dspMaxDays :: Int
    , dspFromAddress :: Address
    , dspTemplate :: TemplateFiles
    } deriving (Show, Eq)

data RemSets = RemSets
    { setsMaxDays :: Int
    , setsWorkDir :: Path Abs Dir
    , setsFromAddress :: Address
    , setsShouldPrint :: ShouldPrint
    , setsTemplateFiles :: TemplateFiles
    } deriving (Show, Eq)

data Dispatch
    = DispatchWaiting DispatchWaitingArgs
    | DispatchNext DispatchNextArgs
    | DispatchRem DispatchRemArgs
    deriving (Show, Eq)
