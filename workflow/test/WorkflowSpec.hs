module WorkflowSpec where

-- import qualified Data.Text as T
import Import
import Test.HUnit.Base
import Test.Hspec
import Workflow
import Workflow.OptParse

spec :: Spec
spec =
    runIO $ do
        putStrLn ""
        workflowPath <- resolveDir' "../test_resources/workflow"
        (waitingHeadings, errMess) <-
            getWaitingHeadings workflowPath $ Settings Not
        strings <- getOutput waitingHeadings
        -- assertEqual
        --     "There are 22 waiting-tasks in the example directory"
        --     (length waitingHeadings)
        --     22
        assertEqual "There are error messages!" errMess ""
        assertEqual
            "Testing the function \"between\""
            (between
                 (/= '[')
                 (/= ']')
                 "- State \"WAITING\"    from \"TODO\"       [2017-01-27 Thu 18:47]")
            "2017-01-27 Thu 18:47"
        -- Tests getWaitingHeadings, filesIO, getContent, isWaiting,
        -- getHeadings, addSth, docToHeading, getAllHeadings
        assertEqual
            "The first task which should be printed"
            (head strings)
            "acc.org: WAITING for the internet company to reply about the wrong invoice.: 29 days"
        -- Checks the exact printed version of one task.
        -- This tests getDate, toWaitingTask, getOutput etc.
        {-case parseRelFile "projects/jobhunt.org" of
            Nothing -> die "\"projects/jobhunt.org\" is not a relative path!"
            Just relPath -> do
                let fileName = workflowPath </> relPath
                contents <- getContent fileName -- contents :: (Text, Path Rel File)
                case getHeadings contents of
                    Left errMessage -> die errMessage
                    Right listHeadings -> mapM_ print $ filter (isWaiting . fst) listHeadings-}
