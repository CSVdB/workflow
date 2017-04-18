{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow where

import qualified Data.ByteString as BS
import Data.OrgMode.Parse
import qualified Data.Text as T
import Data.Time.Clock
import Import
import Options.Applicative.Extra
import Workflow.OptParse

type Text = T.Text

type ByteString = BS.ByteString

workflow :: IO ()
workflow = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchWaiting workflowPath) = waiting workflowPath

waiting :: Path Abs Dir -> Settings -> IO ()
waiting workflowPath _ = do
    files <- filesIO workflowPath
    textList <- mapM getContent files -- textList :: [(ByteString, Path Rel File)]
    let headings = filter (isWaiting . fst) $ foldMap toHeading textList
    let tasks = sort $ fmap toTask headings
    output <- mapM toString tasks
    mapM_ putStrLn output

filesIO :: Path Abs Dir -> IO [Path Abs File]
filesIO workPath = do
    (_, files) <- listDirRecur workPath
    let endsInOrg file = ".org" == drop (length file - 4) file
    pure $ filter (endsInOrg . toFilePath) files

getContent :: Path Abs File -> IO (ByteString, Path Rel File)
getContent filePath = do
    text <- BS.readFile $ toFilePath filePath
    pure (text, filename filePath)

isWaiting :: Heading -> Bool
isWaiting Heading {..} = keyword == Just (StateKeyword "WAITING")

toString :: WaitingTask -> IO String
toString WaitingTask {..} = do
    currentTime <- getCurrentTime
    let file = fromRelFile orgFile
    let nOfDays = (floor $ (/ nominalDay) $ diffUTCTime currentTime date) :: Int
    pure $
        file ++
        ": " ++
        "WAITING for an update from " ++ name ++ ": " ++ show nOfDays ++ " days"

-- | One day in 'NominalDiffTime'.
nominalDay :: NominalDiffTime
nominalDay = 86400

data WaitingTask = WaitingTask
    { date :: UTCTime
    , orgFile :: Path Rel File
    , name :: String
    } deriving (Show, Eq)

instance Ord WaitingTask where
    (<=) task1 task2 = date task1 <= date task2

toTask :: (Heading, Path Rel File) -> WaitingTask
toTask _ = undefined

toHeading :: (ByteString, Path Rel File) -> [(Heading, Path Rel File)]
toHeading _ = undefined

-- let parser = parseDocument ["WAITING", "TODO", "CANCELLED", "DONE"]
--     doc = maybeResult $ parse parser text
toTasks :: Text -> ParserResult [Heading]
toTasks _ = undefined
-- parseDocument :: [Text] -> Parser Text Document
-- documentHeadings :: Document -> [Heading]
