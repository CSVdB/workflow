{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workflow where

import Data.Attoparsec.Text hiding (takeWhile)
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Format
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
waiting workflowPath settings@Settings {..} = do
    (waitingHeadings, errMess) <- getWaitingHeadings workflowPath settings
    case shouldPrintSettings of
        Error -> die errMess
        Warning -> do
            putStrLn errMess
            output waitingHeadings
        Not -> output waitingHeadings

output :: [(Heading, Path Rel File)] -> IO ()
output waitingHeadings = do
    result <- getOutput waitingHeadings
    mapM_ putStrLn result

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

getOutput :: [(Heading, Path Rel File)] -> IO [String]
getOutput waitingHeadings =
    let tasks =
            reverse $
            sortOn date $ catMaybes $ fmap toWaitingTask waitingHeadings
    in mapM toString tasks

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
isWaiting Heading {..} =
    keyword == Just StateKeyword {unStateKeyword = "WAITING"}

toString :: WaitingTask -> IO String
toString WaitingTask {..} =
    let file = fromRelFile orgFile
        outputString = file ++ ": " ++ "WAITING " ++ description
    in case date of
           Nothing -> pure outputString
           Just realDate -> do
               currentTime <- getCurrentTime
               let nOfDays =
                       (floor $
                        (/ nominalDay) $ diffUTCTime currentTime realDate) :: Int
               pure $ outputString ++ ": " ++ show nOfDays ++ " days"

-- | One day in 'NominalDiffTime'.
nominalDay :: NominalDiffTime
nominalDay = 86400

data WaitingTask = WaitingTask
    { date :: Maybe UTCTime
    , orgFile :: Path Rel File
    , description :: String
    } deriving (Show, Eq)

toWaitingTask :: (Heading, Path Rel File) -> Maybe WaitingTask
toWaitingTask (heading@Heading {..}, filePath) =
    if isWaiting heading
        then let date = getDate heading
             in Just $ WaitingTask date filePath (T.unpack title)
        else Nothing

getDate :: Heading -> Maybe UTCTime
getDate Heading {..} =
    let stringDate =
            between (/= '[') (/= ']') $ T.unpack $ sectionParagraph section
        format = "%F %a %R"
    in parseTimeM True defaultTimeLocale format stringDate

between :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
-- Extracts the sublist strictly between the first element which doens't satisfy
-- begin and the first one after that doesn't satisfy end.
between begin end list =
    case dropWhile begin list of
        [] -> []
        _:xs -> takeWhile end xs

getHeadings :: (Text, Path Rel File) -> Either String [(Heading, Path Rel File)]
getHeadings (content, file) =
    case getDocument content of
        Left errMess -> Left errMess
        Right doc -> Right $ addSth (docToHeading doc) file

addSth :: [a] -> b -> [(a, b)]
addSth [] _ = []
addSth (x:xs) addition = (x, addition) : addSth xs addition

docToHeading :: Document -> [Heading]
docToHeading doc = getAllHeadings $ documentHeadings doc

getAllHeadings :: [Heading] -> [Heading]
getAllHeadings [] = []
getAllHeadings (x:xs) = x : getAllHeadings (subHeadings x ++ xs)

getDocument :: Text -> Either String Document
getDocument content =
    let parser =
            parseDocument
                ["WAITING", "TODO", "CANCELLED", "DONE", "READY", "NEXT"]
        result = parseOnly parser content :: Either String Document
    in case result of
           Left errorMessage -> Left errorMessage
           Right doc -> Right doc
