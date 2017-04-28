{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workflow.OptParse.Types where

import Data.Configurator.Types
import qualified Data.Text as T
import Introduction

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command =
    CommandWaiting
    deriving (Show, Eq)

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

data Flags = Flags
    { workflowDir :: Maybe FilePath
    , configFile :: Maybe FilePath
    , shouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data Configuration = Configuration
    { workDirConfig :: FilePath
    , shouldPrintConfig :: ShouldPrint
    } deriving (Show, Eq)

newtype Settings = Settings
    { shouldPrintSettings :: ShouldPrint
    } deriving (Show, Eq)

newtype Dispatch =
    DispatchWaiting (Path Abs Dir)
    deriving (Show, Eq)
