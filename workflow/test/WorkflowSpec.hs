module WorkflowSpec where

import Data.Time.Calendar
import Data.Time.Clock
import Import
import Test.Hspec
import Workflow.OptParse
import Workflow.Waiting

findWorkDir :: IO (Path Abs Dir)
findWorkDir = resolveDir' "../test_resources/workflow"

spec :: Spec
spec =
    parallel $ do
        describe "waitingTasks" $
            it "finds all waiting tasks" $ do
                workflowPath <- findWorkDir
                (waitingHeadings, _) <- getWaitingHeadings workflowPath Settings
                length waitingHeadings `shouldBe` 1
        describe "errorMessages" $
            it "generates no error messages" $ do
                workflowPath <- findWorkDir
                (_, errMess) <- getWaitingHeadings workflowPath Settings
                errMess `shouldBe` []
        describe "testGetDate" $
            it "parses the date and time" $
            between
                (/= '[')
                (/= ']')
                "- State \"WAITING\"    from \"TODO\"       [2017-01-27 Thu 18:47]" `shouldBe`
            "2017-01-27 Thu 18:47"
        describe "formatting" $
            it "formats the output correctly" $ do
                let day = fromGregorian 2017 04 27
                let seconds = 1
                let time = UTCTime day seconds
                workflowPath <- findWorkDir
                (waitingHeadings, _) <- getWaitingHeadings workflowPath Settings
                let strings = headingsToString time waitingHeadings
                last (lines strings) `shouldBe`
                    "projects/test.org WAITING a waiting task 33 days"
        describe "hiddenFiles" $
            it "knows to ignore hidden files" $ do
                dirPath <- resolveDir' "../test_resources/hiddenFiles/"
                ensureDir dirPath
                hiddenFile <- resolveFile dirPath ".iAmHidden.org"
                writeFile
                    (fromAbsFile hiddenFile)
                    "* WAITING THIS SHOULD NOT APPEAR"
                waitingHeadings <- getWaitingHeadings dirPath Settings
                fst waitingHeadings `shouldBe` []
