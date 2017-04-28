module WorkflowSpec where

import Data.OrgMode.Parse
import Data.Time.Calendar
import Data.Time.Clock
import Import
import Test.HUnit.Base hiding (Path)
import Test.Hspec
import Workflow
import Workflow.OptParse

findWorkflowPath :: IO (Path Abs Dir)
findWorkflowPath = resolveDir' "../test_resources/workflow"

findErrMess :: IO String
findErrMess = do
    workflowPath <- findWorkflowPath
    fmap snd $ getWaitingHeadings workflowPath $ Settings Not

findWaitingHeadings :: IO [(Heading, Path Rel File)]
findWaitingHeadings = do
    workflowPath <- findWorkflowPath
    fmap fst $ getWaitingHeadings workflowPath $ Settings Not

findString :: UTCTime -> IO String
findString time = do
    workflowPath <- findWorkflowPath
    (waitingHeadings, _) <- getWaitingHeadings workflowPath $ Settings Not
    let result = getOutput time waitingHeadings
    pure $ boxFormat result

spec :: Spec
spec =
    parallel $ do
        describe "waitingTasks" $
            it "finds all waiting tasks" $ do
                waitingHeadings <- findWaitingHeadings
                assertEqual
                    "There are 22 waiting-tasks in the example directory"
                    (length waitingHeadings)
                    22
        describe "errorMessages" $
            it "generates no error messages" $ do
                errMess <- findErrMess
                assertEqual "There are error messages!" errMess ""
        describe "testGetDate" $
            it "parses the date and time" $
            assertEqual
                "Testing the function \"between\""
                (between
                     (/= '[')
                     (/= ']')
                     "- State \"WAITING\"    from \"TODO\"       [2017-01-27 Thu 18:47]")
                "2017-01-27 Thu 18:47"
        describe "formatting" $
            it "formats the output correctly" $ do
                let day = fromGregorian 2017 04 27
                let seconds = 1
                let time = UTCTime day seconds
                strings <- findString time
                assertEqual
                    "The first task which should be printed"
                    (head $ lines strings)
                    "acc.org              WAITING for the internet company to reply about the wrong invoice.             28 days"
