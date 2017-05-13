{-# LANGUAGE OverloadedStrings #-}

module WorkflowSpec where

import qualified Data.HashMap.Strict as HM
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Import
import Network.Mail.Mime
import Test.Hspec
import Workflow.Next
import Workflow.OptParse
import Workflow.Reminders
import Workflow.Utils
import Workflow.Waiting

findWorkDir :: IO (Path Abs Dir)
findWorkDir = resolveDir' "../test_resources/workflow"

getCurrentTimeHeadingFromTime :: ZonedTime -> Heading
getCurrentTimeHeadingFromTime zonedTime =
    let localTime = zonedTimeToLocalTime zonedTime
        dateString = formatTime defaultTimeLocale "%F %a %R" localTime
        hashmap =
            case propertyEmailAddressNames of
                key:_ ->
                    HM.insert key emailAddress $
                    HM.singleton propertyMaxDaysName "5"
                _ -> HM.empty
    in Heading
       { level = Level 1
       , keyword = Just StateKeyword {unStateKeyword = "WAITING"}
       , priority = Nothing
       , title = "a waiting task"
       , stats = Nothing
       , tags = []
       , section =
             Section
             { sectionPlannings = Plns HM.empty
             , sectionClocks = []
             , sectionProperties = hashmap
             , sectionParagraph =
                   T.pack $
                   concat
                       [ "    - State \"WAITING\"    from \"TODO\"       ["
                       , dateString
                       , "]\n"
                       ]
             }
       , subHeadings = []
       }

getCurrentTimeHeading :: IO (Heading, Path Rel File)
getCurrentTimeHeading = do
    workDir <- resolveDir' "test_resources/currentTime"
    filePath <- resolveFile workDir $ T.unpack currentOrgFile
    file <- stripDir workDir filePath
    currentZonedTime <- getZonedTime
    pure (getCurrentTimeHeadingFromTime currentZonedTime, file)

currentOrgFile :: Text
currentOrgFile = "currentTime.org"

getWaitingTasksAsString :: IO String
getWaitingTasksAsString =
    let timeLocalDay = fromGregorian 2017 04 27
        timeLocalTimeOfDay = TimeOfDay {todHour = 0, todMin = 0, todSec = 1}
        time = LocalTime timeLocalDay timeLocalTimeOfDay
    in do timezone <- getCurrentTimeZone
          workflowPath <- findWorkDir
          (waitingHeadings, _) <- getWaitingHeadings workflowPath Settings
          pure $
              waitingHeadingsToString (ZonedTime time timezone) waitingHeadings

getNextTasks :: IO (String, [[String]])
getNextTasks = do
    workflowPath <- findWorkDir
    files <- getOrgFilesFromDirRecur workflowPath
    result <- mapM (pathToTableOfNexts workflowPath) files
    let (listErrMess, tablesOfNexts) = partitionEithers result
    pure (concat listErrMess, concat tablesOfNexts)

taskMaxDays :: Int
taskMaxDays = 5

emailAddress :: Text
emailAddress = "someone@whatever.com"

address :: Address
address = Address Nothing emailAddress

spec :: Spec
spec =
    parallel $ do
        describe "waitingTasks" $
            it "Waiting finds all WAITING tasks" $ do
                workflowPath <- findWorkDir
                (waitingHeadings, _) <- getWaitingHeadings workflowPath Settings
                length waitingHeadings `shouldBe` 4
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
                    "projects/test.org WAITING an older waiting task              61 days"
        describe "waitingHiddenFiles" $
            it "Workflow knows to ignore hidden files" $ do
                dirPath <- resolveDir' "../test_resources/hiddenFiles/"
                ensureDir dirPath
                hiddenFile <- resolveFile dirPath ".iAmHidden.org"
                writeFile
                    (fromAbsFile hiddenFile)
                    "* WAITING THIS SHOULD NOT APPEAR"
                files <- getOrgFilesFromDirRecur dirPath
                files `shouldBe` []
        describe "timezones" $
            it "Workflow doesn't have a problem with timezones" $ do
                heading <- getCurrentTimeHeading
                strings <- headingsToString [heading]
                let output =
                        T.unpack currentOrgFile ++
                        " WAITING a waiting task 0 days"
                head (lines strings) `shouldBe` output
        describe "nextTasks" $
            it "finds all NEXT tasks" $ do
                (_, tableOfNexts) <- getNextTasks
                length tableOfNexts `shouldBe` 1
        describe "nextErrorMessages" $
            it "Next generates the correct error messages" $ do
                (errMess, _) <- getNextTasks
                errMess `shouldBe` "projects/noNext.org has no next task!"
        describe "nextFormatting" $
            it "Next formats correctly" $ do
                (_, tableOfNexts) <- getNextTasks
                let strings = formatStringAsTable tableOfNexts
                strings `shouldBe` "projects/test.org NEXT a next task\n"
        describe "ageOfTaskInDays" $
            it "Can find the age of tasks in number of days correctly" $ do
                heading <- getCurrentTimeHeading
                currentZonedTime <- getZonedTime
                let nOfDays = ageOfTaskInDays currentZonedTime heading
                nOfDays `shouldBe` Right 0
        describe "headingProperties" $
            it "Can read out the properties of a Heading" $ do
                heading <- getCurrentTimeHeading
                let headingProperties =
                        getHeadingPropertiesFromHeading $ fst heading
                when
                    (headingProperties /=
                     HeadingProperties (Just address) (Just taskMaxDays)) $
                    expectationFailure
                        "'propertyEmailAddressNames' might be an empty list."
        describe "dealWithMails" $
            it "Can create an email and transform it into Text" $ do
                heading <- getCurrentTimeHeading
                let fromAddress = Address Nothing "myemail@something.com"
                let toAddressMaybe =
                        headingReceiver . getHeadingPropertiesFromHeading $
                        fst heading
                case toAddressMaybe of
                    Nothing ->
                        die "Can't read out an email address from the heading!"
                    Just toAddress ->
                        let text =
                                mailToText $
                                createEmail (fst heading) fromAddress toAddress
                        in text `shouldBe`
                           "Do you want to send the following email?\nFrom: myemail@something.com\nTo: someone@whatever.com\nSubject: reminder\nBody:\nThis is a reminder email.\n"
