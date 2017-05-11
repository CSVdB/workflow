{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow.Next where

import Data.OrgMode.Parse
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Import
import Workflow.OptParse
import Workflow.Utils

next :: Path Abs Dir -> [Path Abs File] -> ShouldPrint -> Settings -> IO ()
next projectDir projectFiles shouldPrint _ = do
    (errMess, tablesOfNexts) <-
        partitionEithers <$> mapM (pathToTableOfNexts projectDir) projectFiles
    let tableOfNexts = concat tablesOfNexts
    printErrMess errMess shouldPrint
    putStr $ formatStringAsTable tableOfNexts

pathToTableOfNexts :: Path Abs Dir
                   -> Path Abs File
                   -> IO (Either String [[String]])
pathToTableOfNexts workDir filePath = do
    content <- T.readFile $ toFilePath filePath
    case getDocument content of
        Left errMess -> pure $ Left errMess
        Right doc -> do
            fileName <- stripDir workDir filePath
            pure $ docToNextTasksStrings fileName doc

docToNextTasksStrings :: Path Rel File -> Document -> Either String [[String]]
docToNextTasksStrings path doc =
    let headings = docToHeading doc
    in case filter isNext headings of
           [] -> Left $ fromRelFile path ++ " has no next task!"
           list ->
               Right $ nextHeadingToStrings path <$> filter shouldBePrinted list

isNext :: Heading -> Bool
isNext Heading {..} =
    elem keyword $
    map (Just . StateKeyword) ["NEXT", "STARTED", "WAITING", "READY"]

shouldBePrinted :: Heading -> Bool
shouldBePrinted Heading {..} =
    elem keyword $ map (Just . StateKeyword) ["NEXT", "STARTED"]

nextHeadingToStrings :: Path Rel File -> Heading -> [String]
nextHeadingToStrings path heading@Heading {..} =
    if shouldBePrinted heading
        then [ fromRelFile path
             , (T.unpack . unStateKeyword . fromJust) keyword ++
               " " ++ T.unpack title
             ]
        else []
