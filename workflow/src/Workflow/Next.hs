{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow.Next where

import Data.OrgMode.Parse
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Import
import Workflow.OptParse
import Workflow.Utils

next :: Path Abs Dir -> ShouldPrint -> Settings -> IO ()
next workDir shouldPrint _ = do
    files <- getOrgfilesFromDir workDir
    result <- mapM (pathToTableOfNexts workDir) files
    let (listErrMess, tablesOfNexts) = partitionEithers result
    let errMess = concat listErrMess
    let tableOfNexts = concat tablesOfNexts
    case shouldPrint of
        Error -> die errMess
        Warning -> putStr errMess
        Not -> pure ()
    putStr $ formatStringAsTable $ reverse $ sortOn noNextsFound tableOfNexts

noNextsFound :: [String] -> Bool
noNextsFound (_:descr:_) = descr == "this file has no next task"
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
            pure $ Right $ printNextsFromDoc fileName doc

printNextsFromDoc :: FilePath -> Document -> [[String]]
printNextsFromDoc path doc =
    let headings = docToHeading doc
    in case filter isNext headings of
           [] -> pure [path, "This file has no next task"]
           list -> fmap (printNext path) list

isNext :: Heading -> Bool
isNext Heading {..} =
    keyword `elem`
    [ Just StateKeyword {unStateKeyword = "NEXT"}
    , Just StateKeyword {unStateKeyword = "STARTED"}
    , Just StateKeyword {unStateKeyword = "WAITING"}
    , Just StateKeyword {unStateKeyword = "READY"}
    ]

printNext :: FilePath -> Heading -> [String]
printNext path Heading {..} = [path, T.unpack title]
