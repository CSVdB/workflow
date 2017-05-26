{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workflow.OptParse.Types where

import Data.Configurator.Types
import qualified Data.Text as T
import Data.Text (Text)
import Import
import Network.Mail.Mime

data ShouldPrint
    = Error
    | Warning
    | Not
    deriving (Show, Eq)

data Extension
    = Plain
    | Html
    deriving (Show, Eq)

data FileWithExtension = FileWithExtension
    { ext :: Extension
    , file :: Path Abs File
    } deriving (Show, Eq)

data MailTemplate = MailTemplate
    { headerFile :: Path Abs File
    , bodyFile :: FileWithExtension
    , altBodyFile :: Maybe FileWithExtension
    } deriving (Show, Eq)

data MustachedMailTemplate = MustachedMailTemplate
    { mustachedHeaderFile :: Path Abs File
    , body :: (Extension, Text)
    , altBody :: Maybe (Extension, Text)
    } deriving (Show, Eq)

getShouldPrint :: String -> Maybe ShouldPrint
getShouldPrint "error" = Just Error
getShouldPrint "nothing" = Just Not
getShouldPrint "warning" = Just Warning
getShouldPrint _ = Nothing

instance Configured ShouldPrint where
    convert (String string) = getShouldPrint $ T.unpack string
    convert _ = Nothing

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype WaitingArgsCommand = WaitingArgsCommand
    { cmdWorkDirPath :: Maybe FilePath
    } deriving (Show, Eq)

newtype NextArgsCommand = NextArgsCommand
    { cmdProjectsGlob :: Maybe String
    } deriving (Show, Eq)

data RemArgsCommand = RemArgsCommand
    { cmdWaitArgs :: WaitingArgsCommand
    , cmdMaxDays :: Maybe Int
    , cmdFromAddress :: Maybe Text
    , cmdMailSenderName :: Maybe String
    , cmdTemplateFile :: Maybe FilePath
    } deriving (Show, Eq)

data Command
    = CommandWaiting WaitingArgsCommand
    | CommandNext NextArgsCommand
    | CommandRem RemArgsCommand
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

dftShouldPrint :: ShouldPrint
dftShouldPrint = Warning

data WaitingArgsDispatch = WaitingArgsDispatch
    { dspWorkDir :: Path Abs Dir
    , dspWaitingShouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data NextArgsDispatch = NextArgsDispatch
    { dspProjectDir :: Path Abs Dir
    , dspProjectFiles :: [Path Abs File]
    , dspNextShouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data RemArgsDispatch = RemArgsDispatch
    { dspWaitArgs :: WaitingArgsDispatch
    , maxDays :: Int
    , dspFromAddress :: Address
    , dspTemplate :: MailTemplate
    } deriving (Show, Eq)

data Dispatch
    = DispatchWaiting WaitingArgsDispatch
    | DispatchNext NextArgsDispatch
    | DispatchRem RemArgsDispatch
    deriving (Show, Eq)
