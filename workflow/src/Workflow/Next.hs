{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow.Next where

import Data.OrgMode.Parse
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Import
import Workflow.OptParse
import Workflow.Utils

next :: FilePath -> ShouldPrint -> Settings -> IO ()
next projectDirPath shouldPrint _ = do
    result <-
        case lastN 2 projectDirPath of
            "**" -> do
                projectDir <- parseAbsDir projectDirPath
                files <- getOrgFilesFromDirRecur projectDir
                mapM (pathToTableOfNexts projectDir) files
            _:"*" -> do
                projectDir <- parseAbsDir $ init projectDirPath
                files <- getOrgFilesFromDir projectDir
                mapM (pathToTableOfNexts projectDir) files
            _ -> do
                projectDir <- parseAbsDir projectDirPath
                files <- getOrgFilesFromDir projectDir
                mapM (pathToTableOfNexts projectDir) files
    let (errMess, tablesOfNexts) = partitionEithers result
    let tableOfNexts = concat tablesOfNexts
    printErrMess errMess shouldPrint
    putStr $ formatStringAsTable $ reverse $ sortOn noNextsFound tableOfNexts

lastN :: Int -> [a] -> [a] -- lastN n xs returns the last n elements of xs
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

noNextsFound :: [String] -> Bool
noNextsFound (_:descr:_) = descr == "This file has no next task"
noNextsFound _ = False

pathToTableOfNexts :: Path Abs Dir
                   -> Path Abs File
                   -> IO (Either String [[String]])
pathToTableOfNexts workDir filePath = do
    content <- T.readFile $ toFilePath filePath
    case getDocument content of
        Left errMess -> pure $ Left errMess
        Right doc -> do
            fileNamePath <- stripDir workDir filePath
            let fileName = fromRelFile fileNamePath
            pure $ docToNextTasksStrings fileName doc

docToNextTasksStrings :: FilePath -> Document -> Either String [[String]]
docToNextTasksStrings path doc =
    let headings = docToHeading doc
    in case filter isNext headings of
           [] -> Left $ path ++ " has no next task!"
           list ->
               Right $ nextHeadingToStrings path <$> filter shouldBePrinted list

isNext :: Heading -> Bool
isNext Heading {..} =
    elem keyword $
    map (Just . StateKeyword) ["NEXT", "STARTED", "WAITING", "READY"]

shouldBePrinted :: Heading -> Bool
shouldBePrinted Heading {..} =
    elem keyword $ map (Just . StateKeyword) ["NEXT", "STARTED"]

nextHeadingToStrings :: FilePath -> Heading -> [String]
nextHeadingToStrings path Heading {..} =
    case keyword of
        Just (StateKeyword "NEXT") -> [path, "NEXT " ++ T.unpack title]
        Just (StateKeyword "STARTED") -> [path, "STARTED " ++ T.unpack title]
        _ -> []
