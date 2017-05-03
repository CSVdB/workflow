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
getShouldPrint _ = Nothing

instance Configured ShouldPrint where
    convert (String string) = getShouldPrint $ T.unpack string
    convert _ = Nothing

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandWaiting { workDirCommand :: Maybe FilePath
                    ,  configFile :: Maybe FilePath
                    ,  shouldPrintWaiting :: Maybe ShouldPrint}
    | CommandNext { projectDirCommand :: Maybe FilePath
                 ,  configFile :: Maybe FilePath
                 ,  shouldPrintNext :: Maybe ShouldPrint}
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration
    = ConfigWaiting { workDirConfig :: FilePath
                   ,  shouldPrintConfig :: ShouldPrint}
    | ConfigNext { projectDirConfig :: FilePath
                ,  shouldPrintConfig :: ShouldPrint}
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

defaultShouldPrint :: ShouldPrint
defaultShouldPrint = Warning

data Dispatch
    = DispatchWaiting { workDir :: Path Abs Dir
                     ,  shouldPrintDispatch :: ShouldPrint}
    | DispatchNext { projectDir :: Path Abs Dir
                  ,  shouldPrintDispatch :: ShouldPrint}
    deriving (Show, Eq)
