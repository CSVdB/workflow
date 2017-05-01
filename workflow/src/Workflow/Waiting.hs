{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workflow.Waiting where

import Data.Attoparsec.Text hiding (takeWhile)
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Format
import Data.Tuple
import Import
import System.FilePath.Posix
import Text.PrettyPrint.Boxes
import Workflow.OptParse

waiting :: Path Abs Dir -> ShouldPrint -> Settings -> IO ()
waiting workDir shouldPrint settings = do
    (waitingHeadings, errMess) <- getWaitingHeadings workDir settings
    case shouldPrint of
        Error -> die errMess
        Warning -> do
            putStrLn errMess
            printHeadings waitingHeadings
        Not -> printHeadings waitingHeadings

printHeadings :: [(Heading, Path Rel File)] -> IO ()
printHeadings waitingHeadings = do
    currentTime <- getCurrentTime
    putStrLn $ headingsToString currentTime waitingHeadings

formatString :: [[String]] -> String
formatString list =
    let boxes = transpose $ fmap text <$> list
        table = hsep 1 center1 $ fmap (vcat left) boxes
    in render table

getWaitingHeadings :: Path Abs Dir
                   -> Settings
                   -> IO ([(Heading, Path Rel File)], String)
getWaitingHeadings workDir _ = do
    files <- getFilesFromDir workDir
    textList <- mapM (readFileAndRememberPath workDir) files
    let (errorMessages, listHeadings) =
            partitionEithers $ fmap getHeadings textList
    let headings = concat listHeadings
    let errMess = concat errorMessages
    pure (filter (isWaiting . fst) headings, errMess)

headingsToString :: UTCTime -> [(Heading, Path Rel File)] -> String
headingsToString currentTime waitingHeadings =
    let tasks = sortOn date $ catMaybes $ fmap toWaitingTask waitingHeadings
    in formatString $ fmap (waitingTaskToStrings currentTime) tasks

getFilesFromDir :: Path Abs Dir -> IO [Path Abs File]
getFilesFromDir workPath = do
    (_, files) <- listDirRecur workPath
    let endsInOrg file = ".org" == fileExtension file
    pure $ filter (not . isHidden) $ filter endsInOrg files

isHidden :: Path Abs File -> Bool
isHidden = any startsWithDot . splitDirectories . fromAbsFile

startsWithDot :: FilePath -> Bool
startsWithDot ('.':_) = True
startsWithDot _ = False

readFileAndRememberPath :: Path Abs Dir
                        -> Path Abs File
                        -> IO (Text, Path Rel File)
readFileAndRememberPath workDir filePath = do
    content <- T.readFile $ toFilePath filePath
    relFilePath <- stripDir workDir filePath
    pure (content, relFilePath)

isWaiting :: Heading -> Bool
isWaiting Heading {..} =
    keyword == Just StateKeyword {unStateKeyword = "WAITING"}

waitingTaskToStrings :: UTCTime -> WaitingTask -> [String]
waitingTaskToStrings currentTime WaitingTask {..} =
    let file = fromRelFile orgFile
    in case date of
           Nothing -> [file, "WAITING " ++ description, ""]
           Just realDate ->
               let realDateInUTCTime = addUTCTime (-1 * nominalHour) realDate
               -- timezone = UTC+01:00
                   nOfDays =
                       (floor $
                        diffUTCTime currentTime realDateInUTCTime / nominalDay) :: Int
               in [file, "WAITING " ++ description, show nOfDays ++ " days"]

-- | One day in 'NominalDiffTime'.
nominalDay :: NominalDiffTime
nominalDay = 86400

nominalHour :: NominalDiffTime
nominalHour = 3600

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
docToHeading = getAllHeadings . documentHeadings

getAllHeadings :: [Heading] -> [Heading]
getAllHeadings [] = []
getAllHeadings (x:xs) = x : getAllHeadings (subHeadings x ++ xs)

getDocument :: Text -> Either String Document
getDocument content =
    let parser =
            parseDocument
                ["WAITING", "TODO", "CANCELLED", "DONE", "READY", "NEXT"]
    in parseOnly parser content :: Either String Document
