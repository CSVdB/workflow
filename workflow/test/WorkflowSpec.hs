{-# LANGUAGE OverloadedStrings #-}

module WorkflowSpec where

import qualified Data.HashMap.Strict as HM
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

getFixedZonedTime :: Integer -> IO ZonedTime
getFixedZonedTime int = do
    timezone <- getCurrentTimeZone
    let localtime = LocalTime (ModifiedJulianDay int) (TimeOfDay 0 0 1)
    pure $ ZonedTime localtime timezone

nDays1 :: Integer
nDays1 = 1

nDays2 :: Integer
nDays2 = 6

maxDaysEx :: Int
maxDaysEx = 5

getHeadingFromTime :: ZonedTime -> Heading
getHeadingFromTime zonedTime =
    let localTime = zonedTimeToLocalTime zonedTime
        dateString = formatTime defaultTimeLocale "%F %a %R" localTime
        hashmap =
            case propertyMaxDays of
                keyDays:_ ->
                    HM.insert keyDays (T.pack $ show maxDaysEx) $
                    case propertyReceivers of
                        keyTo:_ ->
                            HM.insert keyTo (nameToAddressText receiverName) $
                            case propertyCc of
                                keyCc:_ ->
                                    HM.insert keyCc (nameToAddressText ccName) $
                                    case propertyBcc of
                                        keyBcc:_ ->
                                            HM.insert
                                                keyBcc
                                                (nameToAddressText bccName) $
                                            case propertyTemplate of
                                                keyTemp:_ ->
                                                    HM.singleton
                                                        keyTemp
                                                        (T.pack templateFile)
                                                _ -> HM.empty
                                        _ -> HM.empty
                                _ -> HM.empty
                        _ -> HM.empty
                _ -> HM.empty
    in Heading
       { level = Level 1
       , keyword = Just StateKeyword {unStateKeyword = "WAITING"}
       , priority = Nothing
       , title = headingTitle
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

headingTitle :: Text
headingTitle = "a waiting task"

getCurrentTimeHeading :: IO (Heading, Path Rel File)
getCurrentTimeHeading = do
    workDir <- resolveDir' "test_resources/currentTime"
    filePath <- resolveFile workDir $ T.unpack currentOrgFile
    orgfile <- stripDir workDir filePath
    currentZonedTime <- getZonedTime
    pure (getHeadingFromTime currentZonedTime, orgfile)

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

nameToAddressText :: Text -> Text
nameToAddressText name = T.concat [name, " <", nameToEmail name, ">"]

nameToAddress :: Text -> Address
nameToAddress name = Address (Just name) $ nameToEmail name

nameToEmail :: Text -> Text
nameToEmail name = T.append name "@something.com"

fromName :: Text
fromName = "Alice"

receiverName :: Text
receiverName = "Bob"

ccName :: Text
ccName = "Cynthia"

bccName :: Text
bccName = "Dieter"

templateFile :: FilePath
templateFile = "template"

getReminderTestFiles :: IO [Path Abs File]
getReminderTestFiles = do
    dir <- resolveDir' "../test_resources/reminders/"
    resolveStringToFiles $ fromAbsDir dir ++ "test"

getMustachedTemplate :: IO (MustachedMailTemplate, Heading, Path Rel File)
getMustachedTemplate = do
    files <- getReminderTestFiles
    zonedTime <- getFixedZonedTime nDays1
    orgfile <- parseRelFile "something.org"
    let heading = getHeadingFromTime zonedTime
    let fromAddress = Address (Just fromName) $ nameToEmail fromName
    case templateToMailTemplate files of
        Left _ -> die "Something went wrong in templateToMailTemplate"
        Right mailTemplate ->
            case getMustache fromAddress "Reminder email" $
                 getHeadingProperties heading of
                Nothing -> die "getMustache couldn't create a MailInfo"
                Just mailInfo -> do
                    mustachedMailTemplateEither <-
                        mustacheToMailTemplate mailInfo mailTemplate
                    case mustachedMailTemplateEither of
                        Left errMess -> die $ unlines errMess
                        Right mustachedTemplate ->
                            pure (mustachedTemplate, heading, orgfile)

spec :: Spec
spec = do
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
                    T.unpack currentOrgFile ++ " WAITING a waiting task 0 days"
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
    describe "calculateAgeOfTask" $
        it "Calculates the age of a task in # days" $ do
            zonedTime <- getFixedZonedTime nDays1
            zonedTime2 <- getFixedZonedTime nDays2
            orgfile <- parseRelFile "something.org"
            let nOfDaysEither =
                    ageOfTaskInDays
                        zonedTime2
                        (getHeadingFromTime zonedTime, orgfile) :: Either String Int
            (toInteger <$> nOfDaysEither) `shouldBe` (Right $ nDays2 - nDays1)
    describe "getHeadingProperties" $
        it "Extracts the HeadingProperties from a Heading" $ do
            zonedTime <- getFixedZonedTime nDays1
            let heading = getHeadingFromTime zonedTime
            getHeadingProperties heading `shouldBe`
                HeadingProperties
                { headingReceivers = Just $ nameToAddressText receiverName
                , headingCc = nameToAddressText ccName
                , headingBcc = nameToAddressText bccName
                , headingMaxDays = Just maxDaysEx
                , headingTemplateFile = Just templateFile
                , headingTaskTitle = headingTitle
                }
    describe "resolveStringToFiles" $
        it "finds the appropriate, existing files given a FilePath" $ do
            files <- getReminderTestFiles
            fromRelFile . filename <$>
                files `shouldBe`
                [ "test.header"
                , "test.txt"
                ]
    describe "templateToMailTemplate" $
        it "generate the MailTemplate" $ do
            files <- getReminderTestFiles
            case files of
                [header, plainFile] ->
                    templateToMailTemplate files `shouldBe`
                    Right
                        (MailTemplate
                             header
                             (FileWithExtension Plain plainFile)
                             Nothing)
                _ ->
                    expectationFailure
                        "Something went wrong in resolveStringToFiles"
    describe "mustache" $
        it "mustaches the MailTemplate correctly" $ do
            (mustachedTemplate, _, _) <- getMustachedTemplate
            headerFileContent <-
                T.readFile $ fromAbsFile $ mustachedHeaderFile mustachedTemplate
            headerFileContent `shouldBe`
                "subject = \"Reminder email\"\nto = \"Bob <Bob@something.com>\"\nfrom = \"Alice <Alice@something.com>\"\ncc = \"Cynthia <Cynthia@something.com>\"\nbcc = \"Dieter <Dieter@something.com>\""
    describe "dealWithMails" $
        it "Can create an email and transform it into Text" $ do
            (mustachedTemplate, heading, orgfile) <- getMustachedTemplate
            mailEither <- createEmail heading orgfile mustachedTemplate
            case mailEither of
                Left errMess -> printErrMess (lines errMess) Error
                Right mail
                            -- destroyFiles mustachedTemplate
                 ->
                    mailToText mail `shouldBe`
                    "Do you want to send the following email?\nFrom: Alice <Alice@something.com>\nTo: Bob <Bob@something.com>\nCC: Cynthia <Cynthia@something.com>\nBCC: Dieter <Dieter@something.com>\nSubject: Reminder email\ntext/plain: Dear Bob,\n\nThis is a reminder email for the task a waiting task.\n\nBest regards,\nAlice\ntext/html: \n"
