{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workflow.OptParse.Types where

import Data.Configurator.Types
import qualified Data.Text as T
import Import

data ShouldPrint
    = Error
    | Warning
    | Not
    deriving (Show, Eq)

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
    { cmdWorkDir :: Maybe FilePath
    } deriving (Show, Eq)

newtype NextArgsCommand = NextArgsCommand
    { cmdProjectDir :: Maybe FilePath
    } deriving (Show, Eq)

data Command
    = CommandWaiting WaitingArgsCommand
    | CommandNext NextArgsCommand
    deriving (Show, Eq)

data Flags = Flags
    { flagsConfigFile :: Maybe FilePath
    , flagsShouldPrint :: Maybe ShouldPrint
    } deriving (Show, Eq)

data Configuration = Configuration
    { cfgWorkDir :: Path Abs Dir
    , cfgProjectDir :: FilePath
    , cfgShouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

defaultShouldPrint :: ShouldPrint
defaultShouldPrint = Warning

data WaitingArgsDispatch = WaitingArgsDispatch
    { dspWworkDir :: Path Abs Dir
    , dspWaitingShouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data NextArgsDispatch = NextArgsDispatch
    { dspProjectDir :: FilePath
    , dspNextShouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data Dispatch
    = DispatchWaiting WaitingArgsDispatch
    | DispatchNext NextArgsDispatch
    deriving (Show, Eq)
