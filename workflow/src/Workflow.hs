{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workflow where

import Data.Attoparsec.Text
import Data.Either
import Data.Maybe
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Tuple
import Import
import Workflow.OptParse

workflow :: IO ()
workflow = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchWaiting workflowPath) = waiting workflowPath

waiting :: Path Abs Dir -> Settings -> IO ()
waiting workflowPath _ = do
    (waitingHeadings, _) <- getWaitingHeadings workflowPath Settings
    let tasks = catMaybes $ fmap toTask waitingHeadings
    let sortedTasks = sortOn date tasks
    output <- mapM toString sortedTasks
    mapM_ putStrLn output

getWaitingHeadings :: Path Abs Dir
                   -> Settings
                   -> IO ([(Heading, Path Rel File)], String)
getWaitingHeadings workflowPath _ = do
    files <- filesIO workflowPath
    textList <- mapM getContent files -- textList :: [(Text, Path Rel File)]
    let (errorMessages, listHeadings) =
            partitionEithers $ fmap getHeadings textList
    let headings = concat listHeadings
    let errMess = concat errorMessages
    pure (filter (isWaiting . fst) headings, errMess)

filesIO :: Path Abs Dir -> IO [Path Abs File]
filesIO workPath = do
    (_, files) <- listDirRecur workPath
    let endsInOrg file = ".org" == fileExtension file
    pure $ filter endsInOrg files

getContent :: Path Abs File -> IO (Text, Path Rel File)
getContent filePath = do
    content <- T.readFile $ toFilePath filePath
    pure (content, filename filePath)

isWaiting :: Heading -> Bool
isWaiting Heading {..} = keyword == Just (StateKeyword "WAITING")

toString :: WaitingTask -> IO String
toString WaitingTask {..} =
    let file = fromRelFile orgFile
        output = file ++ ": " ++ "WAITING for an update from " ++ descr
    in case date of
           Nothing -> pure output
           Just realDate -> do
               currentTime <- getCurrentTime
               let nOfDays =
                       (floor $
                        (/ nominalDay) $ diffUTCTime currentTime realDate) :: Int
               pure $ output ++ ": " ++ show nOfDays ++ " days"

-- | One day in 'NominalDiffTime'.
nominalDay :: NominalDiffTime
nominalDay = 86400

data WaitingTask = WaitingTask
    { date :: Maybe UTCTime
    , orgFile :: Path Rel File
    , descr :: String
    } deriving (Show, Eq)

toTask :: (Heading, Path Rel File) -> Maybe WaitingTask
toTask (heading, filePath) =
    if isWaiting heading
        then let (date, descr) = getDateAndText heading
             in Just $ WaitingTask date filePath descr
        else Nothing

getDateAndText :: Heading -> (Maybe UTCTime, String)
getDateAndText Heading {..} =
    let (description, rest) = T.span (/= '[') title
    in if T.length rest == 0
           then (Nothing, T.unpack description)
           else let date = T.takeWhile (/= ']') $ T.tail rest
                in (formatDate date, T.unpack description)

formatDate :: Text -> Maybe UTCTime
formatDate _ = undefined

getHeadings :: (Text, Path Rel File) -> Either String [(Heading, Path Rel File)]
getHeadings (content, file) =
    case getDocument content of
        Left errMess -> Left errMess
        Right doc -> Right $ addSth (docToHeading doc) file

addSth
    :: forall a b.
       ([a] -> b -> [(a, b)])
addSth [] _ = []
addSth (x:xs) addition = (x, addition) : addSth xs addition

docToHeading :: Document -> [Heading]
docToHeading doc =
    let topHeadings = documentHeadings doc :: [Heading]
    in getAllHeadings topHeadings

getAllHeadings :: [Heading] -> [Heading]
getAllHeadings [] = []
getAllHeadings (x:xs) = x : getAllHeadings (subHeadings x ++ xs)

getDocument :: Text -> Either String Document
getDocument content =
    let parser = parseDocument ["WAITING", "TODO", "CANCELLED", "DONE", "READY"]
        result = parseOnly parser content :: Either String Document
    in case result of
           Left errorMessage -> Left errorMessage
           Right doc -> Right doc
