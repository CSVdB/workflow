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
        output waitingHeadings
        assertEqual
            "There are 22 waiting-tasks in the example directory"
            (length waitingHeadings)
            22
        assertEqual "There are error messages!" errMess ""
        assertEqual
            "Testing the function \"between\""
            (between
                 (/= '[')
                 (/= ']')
                 "- State \"WAITING\"    from \"TODO\"       [2017-01-27 Thu 18:47]")
            "2017-01-27 Thu 18:47"
        assertEqual
            "The first task which should be printed"
            (head strings)
            [ "acc.org"
            , "WAITING for the internet company to reply about the wrong invoice."
            , "29 days"
            ]
