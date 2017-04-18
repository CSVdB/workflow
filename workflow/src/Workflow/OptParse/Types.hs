{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workflow.OptParse.Types where

import Introduction

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command =
    CommandWaiting
    deriving (Show, Eq)

data Flags = Flags
    { workPath :: Maybe FilePath
    , confPath :: Maybe FilePath
    } deriving (Show, Eq)

newtype Configuration = Configuration
    { confDataDir :: FilePath
    } deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

newtype Dispatch =
    DispatchWaiting (Path Abs Dir)
    deriving (Show, Eq)
