{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workflow.Waiting where

import Data.OrgMode.Parse
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Tuple
import Import
import Workflow.OptParse
import Workflow.Utils

waiting :: Path Abs Dir -> ShouldPrint -> Settings -> IO ()
waiting workDir shouldPrint settings = do
    (waitingHeadings, errMess) <- getWaitingHeadings workDir settings
    case shouldPrint of
        Error -> die errMess
        Warning -> putStrLn errMess
        Not -> pure ()
    string <- headingsToString waitingHeadings
    putStrLn string

headingsToString :: [(Heading, Path Rel File)] -> IO String
headingsToString waitingHeadings = do
    currentTime <- zonedTimeToLocalTime <$> getZonedTime
    timezone <- getCurrentTimeZone
    pure $ waitingHeadingsToString timezone currentTime waitingHeadings

getWaitingHeadings :: Path Abs Dir
                   -> Settings
                   -> IO ([(Heading, Path Rel File)], String)
getWaitingHeadings workDir sett = do
    (headings, errMess) <- getHeadingsFromDir workDir sett
    pure (filter (isWaiting . fst) headings, errMess)

waitingHeadingsToString :: TimeZone
                        -> LocalTime
                        -> [(Heading, Path Rel File)]
                        -> String
waitingHeadingsToString timezone currentTime waitingHeadings =
    let tasks = sortOn date $ catMaybes $ fmap toWaitingTask waitingHeadings
    in formatStringAsTable $
       fmap (waitingTaskToStrings timezone currentTime) tasks

isWaiting :: Heading -> Bool
isWaiting Heading {..} =
    keyword == Just StateKeyword {unStateKeyword = "WAITING"}

waitingTaskToStrings :: TimeZone -> LocalTime -> WaitingTask -> [String]
waitingTaskToStrings timezone currentTime WaitingTask {..} =
    let file = fromRelFile orgFile
    in case date of
           Nothing -> [file, "WAITING " ++ description, ""]
           Just realDate ->
               let currentUTCTime = localTimeToUTC timezone currentTime
                   realUTCTime = localTimeToUTC timezone realDate
                   nOfDays =
                       (floor $
                        diffUTCTime currentUTCTime realUTCTime / nominalDay) :: Int
               in [file, "WAITING " ++ description, show nOfDays ++ " days"]

data WaitingTask = WaitingTask
    { date :: Maybe LocalTime
    , orgFile :: Path Rel File
    , description :: String
    } deriving (Show, Eq)

toWaitingTask :: (Heading, Path Rel File) -> Maybe WaitingTask
toWaitingTask (heading@Heading {..}, filePath) =
    if isWaiting heading
        then let date = getDate heading
             in Just $ WaitingTask date filePath (T.unpack title)
        else Nothing
