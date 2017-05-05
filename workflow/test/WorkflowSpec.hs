module WorkflowSpec where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Import
import System.Directory
import Test.Hspec
import Workflow.OptParse
import Workflow.Utils
import Workflow.Waiting

findWorkDir :: IO (Path Abs Dir)
findWorkDir = resolveDir' "../test_resources/workflow"

getWaitingTasksAsString :: IO String
getWaitingTasksAsString =
    let day = fromGregorian 2017 04 27
        time =
            LocalTime
            { localDay = day
            , localTimeOfDay = TimeOfDay {todHour = 0, todMin = 0, todSec = 1}
            }
    in do timezone <- getCurrentTimeZone
          workflowPath <- findWorkDir
          (waitingHeadings, _) <- getWaitingHeadings workflowPath Settings
          pure $ waitingHeadingsToString timezone time waitingHeadings

spec :: Spec
spec =
    parallel $ do
        describe "waitingTasks" $
            it "Waiting finds all WAITING tasks" $ do
                workflowPath <- findWorkDir
                (waitingHeadings, _) <- getWaitingHeadings workflowPath Settings
                length waitingHeadings `shouldBe` 3
        describe "waitingErrorMessages" $
            it "Waiting generates no error messages" $ do
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
        describe "waitingFormatting" $
            it "Waiting formats the output correctly" $ do
                strings <- getWaitingTasksAsString
                head (lines strings) `shouldBe`
                    "projects/test.org WAITING an older waiting task       61 days"
        describe "waitingHiddenFiles" $
            it "Workflow knows to ignore hidden files" $ do
                dirPath <- resolveDir' "../test_resources/hiddenFiles/"
                ensureDir dirPath
                hiddenFile <- resolveFile dirPath ".iAmHidden.org"
                writeFile
                    (fromAbsFile hiddenFile)
                    "* WAITING THIS SHOULD NOT APPEAR"
                files <- getFilesFromDir dirPath
                files `shouldBe` []
        describe "ordering" $
            it "Waiting outputs the WAITING tasks in the correct order" $ do
                strings <- getWaitingTasksAsString
                strings `shouldBe`
                    "projects/test.org WAITING an older waiting task       " ++
                    "61 days\nprojects/test.org WAITING a waiting task" ++
                    "              33 days\nprojects/test.org WAITING a waiting" ++
                    " task without date        \n"
        describe "timezones" $
            it "Workflow doesn't have a problem with timezones" $ do
                currentTime <- zonedTimeToLocalTime <$> getZonedTime
                dirPath <- resolveDir' "../test_resources/timezones/"
                ensureDir dirPath
                timezoneFile <- resolveFile dirPath "timezone.org"
                let currentDate =
                        show (localDay currentTime) ++
                        " Sat " ++
                        formatTime
                            defaultTimeLocale
                            "%R"
                            (localTimeOfDay currentTime)
                let task =
                        "* WAITING timezone\n    - State \"WAITING\"    " ++
                        "from \"TODO\"       [" ++ currentDate ++ "]\n"
                writeFile (fromAbsFile timezoneFile) task
                (headings, _) <- getWaitingHeadings dirPath Settings
                strings <- headingsToString headings
                removePathForcibly $ fromAbsFile timezoneFile
                strings `shouldBe` "timezone.org WAITING timezone 0 days\n"
