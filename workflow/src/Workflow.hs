{-# LANGUAGE OverloadedStrings #-}

module Workflow where

import Introduction

import qualified Data.Text.IO as T
import Data.Time.Clock
import Workflow.OptParse

workflow :: IO ()
workflow = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchWaiting workflowPath) = waiting workflowPath

waiting :: Path Abs Dir -> Settings -> IO ()
waiting workflowPath _ = do
    files <- filesIO workflowPath
    tasks <- extractTasksFromFiles files
    let waitingTasks = sort $ filter isWaiting tasks
    mapM_ putStrLn $ fmap toString waitingTasks

toString :: Task -> String
toString _ = ""

data Status
    = TODO
    | DONE
    | CANCELLED
    | WAITING
    deriving (Show, Eq)

data Task = Task
    { status :: Status
    , description :: String
    , time :: Maybe UTCTime
    } deriving (Show, Eq)

instance Ord Task where
    (<=) _ _ = True

isWaiting :: Task -> Bool
isWaiting task = status task == WAITING

filesIO :: Path Abs Dir -> IO [Path Abs File]
filesIO _ = pure []

extractTasks :: Path Abs File -> IO [Task]
extractTasks filePath = do
    text <- T.readFile (toFilePath filePath)
    pure $ toTasks text

extractTasksFromFiles :: [Path Abs File] -> IO [Task]
extractTasksFromFiles [] = pure []
extractTasksFromFiles (x:xs) = do
    tasks <- extractTasks x
    otherTasks <- extractTasksFromFiles xs
    pure $ tasks ++ otherTasks

toTasks :: Text -> [Task]
toTasks _ = []
