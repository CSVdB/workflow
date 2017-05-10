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
        case (take 2 . reverse) projectDirPath of
            "**" -> do
                projectDir <- parseAbsDir projectDirPath
                files <- getOrgFilesFromDirRecur projectDir
                mapM (pathToTableOfNexts projectDir) files
            '*':_ -> do
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
    putStr $ formatStringAsTable tableOfNexts

lastN :: Int -> [a] -> [a] -- lastN n xs returns the last n elements of xs
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

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
nextHeadingToStrings path heading@Heading {..} =
    if shouldBePrinted heading
        then [ path
             , (T.unpack . unStateKeyword . fromJust) keyword ++
               " " ++ T.unpack title
             ]
        else []
