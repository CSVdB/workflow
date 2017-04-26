module WorkflowSpec where

import Import
import Test.Hspec
import Workflow
import Workflow.OptParse

spec :: Spec
spec =
    runIO $
    case parseAbsDir "/home/nick/Syd/org-mode/examples/workflow" of
        Nothing -> die "This is not a valid path to a directory!"
        Just workflowPath -> do
            (waitingHeadings, errMess) <-
                getWaitingHeadings workflowPath Settings
            mapM_ print waitingHeadings
            print $ length waitingHeadings
            print $ errMess == ""
            -- Add the test "this should be 22",
            -- Tests getWaitingHeadings, filesIO, getContent, isWaiting,
            -- getHeadings, addSth, docToHeading, getAllHeadings
-- Add tests for toTask, toString, getDateAndText, formatDate
