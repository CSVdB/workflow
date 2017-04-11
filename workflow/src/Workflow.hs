module Workflow where

import Introduction

import Workflow.Types
import Workflow.OptParse
import Data.Time.Calendar
import Data.Time.Clock
-- import qualified Data.Text.IO as T

workflow :: IO ()
workflow = do
	(disp, sett) <- getInstructions
	-- Use this dispatch and these settings
	putStrLn "hi"

waiting :: IO ()
waiting = putStrLn "hi"
	-- Use T.readFile to read files
-- This function should make a list of all "waiting" tasks,
-- sort them chronologically, format them into a nice string and print the string.

{-
data Status = TODO | DONE { momentOfClosure :: UTCTime }
              | CANCELLED { momentOfClosure :: UTCTime }
              | WAITING { startMoment :: UTCTime } deriving (Show, Eq)

data Task = Task { status :: Status
                 , description :: String
                 } deriving (Show, Eq)

isWaiting :: Task -> Bool
isWaiting task = status task == WAITING

files :: Configuration -> [FilePath]
files config = []
-- Define files once you implemented how to do deal with Configurations

extractTasks :: FilePath -> [Task]
extractTasks filePath = list where
	list <- do
	tasks <- readFile filePath -- tasks :: Text
    pure $ fmap toTask (splitOn "** " tasks)

toTask :: Text -> Task
-}
