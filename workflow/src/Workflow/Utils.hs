{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workflow.Utils where

import Data.Attoparsec.Text hiding (takeWhile)
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Import
import System.FilePath.Posix
import Text.PrettyPrint.Boxes
import Workflow.OptParse

printErrMess :: [String] -> ShouldPrint -> IO ()
printErrMess [] Error = pure ()
printErrMess errMess Error = die $ unlines errMess
printErrMess errMess Warning = putStr $ unlines errMess
printErrMess _ Not = pure ()

formatStringAsTable :: [[String]] -> String
formatStringAsTable list =
    let boxes = transpose $ fmap text <$> list
        table = hsep 1 center1 $ fmap (vcat left) boxes
    in render table

getHeadingsFromDir :: Path Abs Dir
                   -> Settings
                   -> IO ([(Heading, Path Rel File)], [String])
getHeadingsFromDir workDir _ = do
    files <- getOrgFilesFromDirRecur workDir
    textList <- mapM (readFileAndRememberPath workDir) files
    let (errorMessages, listHeadings) =
            partitionEithers $ fmap getHeadingsFromFile textList
    pure (concat listHeadings, errorMessages)

getOrgFilesFromDir :: Path Abs Dir -> IO [Path Abs File]
getOrgFilesFromDir projectDir = filter isOrgFile . snd <$> listDir projectDir

getOrgFilesFromDirRecur :: Path Abs Dir -> IO [Path Abs File]
getOrgFilesFromDirRecur projectDir =
    filter isOrgFile . snd <$> listDirRecur projectDir

isOrgFile :: Path Abs File -> Bool
isOrgFile file = (not . isHidden) file && fileExtension file == ".org"

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

-- | One day in 'NominalDiffTime'.
nominalDay :: NominalDiffTime
nominalDay = 86400

nominalHour :: NominalDiffTime
nominalHour = 3600

getDate :: Heading -> Maybe LocalTime
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

getHeadingsFromFile :: (Text, Path Rel File)
                    -> Either String [(Heading, Path Rel File)]
getHeadingsFromFile (content, file) =
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
                [ "WAITING"
                , "TODO"
                , "CANCELLED"
                , "DONE"
                , "READY"
                , "NEXT"
                , "STARTED"
                ]
    in parseOnly parser content :: Either String Document
