module Workflow.OptParse.Types where

import Introduction

type Arguments = (Command, Flags)
type Instructions = (Dispatch, Settings)
data Command
    = CommandWaiting
    deriving (Show, Eq)

data Flags
    = Flags
    deriving (Show, Eq)

data Configuration
    = Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchWaiting
    deriving (Show, Eq)

data Settings
    = Settings
    deriving (Show, Eq)
