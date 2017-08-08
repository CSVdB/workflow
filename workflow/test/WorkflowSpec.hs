{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WorkflowSpec where

import Data.Char
import Data.GenValidity

import Data.GenValidity.Time.LocalTime ()
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
import Test.QuickCheck
import Workflow.Next
import Workflow.OptParse
import Workflow.Reminders
import Workflow.Utils
import Workflow.Waiting

findWorkDir :: IO (Path Abs Dir)
findWorkDir = resolveDir' "../test_resources/workflow"

getFixedZonedTime :: Integer -> ZonedTime
getFixedZonedTime int =
    let localTime = getFixedLocalTime int
    in ZonedTime localTime utc

getFixedLocalTime :: Integer -> LocalTime
getFixedLocalTime int = LocalTime (ModifiedJulianDay int) (TimeOfDay 0 0 3)

nDays1 :: Integer
nDays1 = 1

nDays2 :: Integer
nDays2 = 6

maxDaysEx :: Int
maxDaysEx = 5

newtype AlphabeticChar =
    Alpha Char
    deriving (Eq, Show)

instance Validity AlphabeticChar where
    isValid (Alpha s) = isAlpha s

instance GenUnchecked AlphabeticChar where
    genUnchecked = Alpha <$> arbitrary

instance GenValid AlphabeticChar where
    genValid = Alpha <$> arbitrary `suchThat` isValid

instance GenInvalid AlphabeticChar

newtype AlphabeticString =
    AlphaString String
    deriving (Eq, Show)

data HeadingInfo = HeadingInfo
    { receiverNameInfo :: Text
    , ccNameInfo :: Text
    , bccNameInfo :: Text
    , maxDaysExInfo :: Int
    , templateFileInfo :: FilePath
    , headingTitleInfo :: Text
    , zonedTimeInfo :: ZonedTime
    , stateKeywordInfo :: StateKeyword
    } deriving (Show)

statekeywords :: [StateKeyword]
statekeywords = StateKeyword <$> ["WAITING", "TODO", "READY", "DONE", "STARTED"]

generateHeadingInfo :: Gen HeadingInfo
generateHeadingInfo =
    let alphaStringGenerator =
            genListOf (arbitrary `suchThat` isAlpha) `suchThat` (not . null)
        alphaTextGenerator = T.pack <$> alphaStringGenerator
        stateKeywordGen = elements statekeywords
    in HeadingInfo <$> alphaTextGenerator <*> alphaTextGenerator <*>
       alphaTextGenerator <*>
       arbitrarySizedNatural <*>
       alphaStringGenerator <*>
       alphaTextGenerator <*>
       genValid <*>
       stateKeywordGen

getHeadingFromData :: HeadingInfo -> Heading
getHeadingFromData HeadingInfo {..} =
    let hashmap =
            HM.fromList $
            concat
                [ (swap . (,) (T.pack $ show maxDaysExInfo)) <$> propertyMaxDays
                , (swap . (,) (nameToAddressText receiverNameInfo)) <$>
                  propertyReceivers
                , (swap . (,) (nameToAddressText ccNameInfo)) <$> propertyCc
                , (swap . (,) (nameToAddressText bccNameInfo)) <$> propertyBcc
                , (swap . (,) (T.pack templateFileInfo)) <$> propertyTemplate
                ]
        localTime = zonedTimeToLocalTime zonedTimeInfo
        dateString = formatTime defaultTimeLocale "%F %a %R" localTime
    in Heading
       { level = Level 1
       , keyword = Just stateKeywordInfo
       , priority = Nothing
       , title = headingTitleInfo
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

getHeadingPropertiesFromFunction :: HeadingInfo -> HeadingProperties
getHeadingPropertiesFromFunction HeadingInfo {..} =
    HeadingProperties
    { headingReceivers = Just $ nameToAddressText receiverNameInfo
    , headingCc = nameToAddressText ccNameInfo
    , headingBcc = nameToAddressText bccNameInfo
    , headingMaxDays = Just maxDaysExInfo
    , headingTemplateFile = Just templateFileInfo
    , headingTaskTitle = headingTitleInfo
    }

getHeadingFromTime :: ZonedTime -> Heading
getHeadingFromTime zonedTime =
    getHeadingFromData $
    HeadingInfo
        receiverName
        ccName
        bccName
        maxDaysEx
        templateFile
        headingTitle
        zonedTime
        stateKeyword

stateKeyword :: StateKeyword
stateKeyword = StateKeyword "WAITING"

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
    dir <- resolveDir' "../test_resources/workflow/"
    files <- mapM (resolveFile dir) ["projects/test.org", "projects/noNext.org"]
    result <- mapM (pathToTableOfNexts dir) files
    let (listErrMess, tablesOfNexts) = partitionEithers result
    pure (concat listErrMess, concat tablesOfNexts)

taskMaxDays :: Int
taskMaxDays = 5

nameToAddressText :: Text -> Text
nameToAddressText name = T.concat [name, " <", nameToEmail name, ">"]

nameToAddress :: Text -> Address
nameToAddress name = Address (Just name) $ nameToEmail name

nameToEmail :: Text -> Text
nameToEmail name = T.append name "@example.com"

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
    let zonedTime = getFixedZonedTime nDays1
    orgfile <- parseRelFile "something.org"
    let heading = getHeadingFromTime zonedTime
    case templateToMailTemplate files of
        Left errMess ->
            die $
            unlines
                [ "templateToMailTemplate gave the following error message:"
                , errMess
                ]
        Right mailTemplate ->
            case getMustache fromAddress "Reminder email" $
                 getHeadingProperties heading of
                Nothing -> die "getMustache couldn't create a MailInfo"
                Just mailinfo -> do
                    mustachedMailTemplateEither <-
                        mustacheToMailTemplate mailinfo mailTemplate
                    case mustachedMailTemplateEither of
                        Left errMess -> die $ unlines errMess
                        Right mustachedTemplate ->
                            pure (mustachedTemplate, heading, orgfile)

mailSubject :: Text
mailSubject = "This is a reminder email"

fromAddress :: Address
fromAddress = nameToAddress fromName

mailInfo :: MailInfo
mailInfo =
    MailInfo
    { to = nameToAddressText receiverName
    , cc = nameToAddressText ccName
    , bcc = nameToAddressText bccName
    , from = nameToAddressText fromName
    , subject = mailSubject
    , taskTitle = headingTitle
    }

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
        let nDays = nDays1
            zonedTime = getFixedZonedTime nDays
            header = getHeadingFromTime zonedTime
            localTime = getFixedLocalTime nDays
            dropSeconds TimeOfDay {..} = TimeOfDay todHour todMin 0
        in getDate header `shouldBe`
           Just
               (LocalTime
                    (localDay localTime)
                    (dropSeconds $ localTimeOfDay localTime))
    describe "waitingFormatting" $
        it "Waiting formats the output correctly" $ do
            strings <- getWaitingTasksAsString
            case lines strings of
                firstTask:_ ->
                    firstTask `shouldBe`
                    "projects/test.org WAITING an older waiting task              61 days"
                [] ->
                    expectationFailure
                        "getWaitingTasksAsString returns an empty string"
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
    describe "isOrgFile" $
        it "tests isOrgFile" $ do
            dir <- resolveDir' "../test_resources/hiddenFiles/"
            file <- resolveFile dir ".iAmHidden.org"
            shouldNotSatisfy file isOrgFile
    describe "timezones" $
        it "Workflow doesn't have a problem with timezones" $ do
            heading <- getCurrentTimeHeading
            strings <- headingsToString [heading]
            let output =
                    T.unpack currentOrgFile ++ " WAITING a waiting task 0 days"
            case lines strings of
                firstHeading:_ -> firstHeading `shouldBe` output
                [] ->
                    expectationFailure
                        "headingsToString applied to a list of one heading returns an empty String."
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
            formatStringAsTable tableOfNexts `shouldBe`
                "projects/test.org NEXT a next task\n"
    describe "ageOfTaskInDays" $
        it "Can find the age of tasks in number of days correctly" $
        let zonedTime = getFixedZonedTime nDays1
            zonedTime2 = getFixedZonedTime nDays2
            heading = getHeadingFromTime zonedTime
        in do orgfile <- parseRelFile templateFile
              let nOfDays = ageOfTaskInDays zonedTime2 (heading, orgfile)
              toInteger <$> nOfDays `shouldBe` Right (nDays2 - nDays1)
    describe "calculateAgeOfTask" $
        it "Calculates the age of a task in # days" $ do
            let zonedTime = getFixedZonedTime nDays1
            let zonedTime2 = getFixedZonedTime nDays2
            orgfile <- parseRelFile "something.org"
            let nOfDaysEither =
                    ageOfTaskInDays
                        zonedTime2
                        (getHeadingFromTime zonedTime, orgfile) :: Either String Int
            (toInteger <$> nOfDaysEither) `shouldBe` (Right $ nDays2 - nDays1)
    describe "getHeadingProperties using property testing" $
        it "Extracts the HeadingProperties from a Heading" $
        forAll
            generateHeadingInfo
            (\x ->
                 getHeadingProperties (getHeadingFromData x) ==
                 getHeadingPropertiesFromFunction x)
    describe "resolveStringToFiles" $
        it "finds the appropriate, existing files given a FilePath" $ do
            files <- getReminderTestFiles
            fromRelFile . filename <$>
                files `shouldBe` ["test.header", "test.txt"]
    describe "templateToMailTemplate" $
        it "generate the MailTemplate" $ do
            files <- getReminderTestFiles
            case files of
                [header, plainFile] ->
                    templateToMailTemplate files `shouldBe`
                    Right (MailTemplate header plainFile Nothing)
                _ ->
                    expectationFailure $
                    "resolveStringToFiles returned files " ++ show files ++ "."
    describe "getMustache" $
        it "Obtains the MailInfo correctly from the heading" $ do
            headProp <- getHeadingProperties . fst <$> getCurrentTimeHeading
            getMustache fromAddress mailSubject headProp `shouldBe`
                Just mailInfo
    describe "mustacheFile" $
        it "mustache a file" $ do
            path <- resolveFile' "../test_resources/reminders/temp.header"
            mustachedText <- mustacheFile mailInfo path
            let text =
                    T.concat
                        [ "subject = \"Reminder email\"\n"
                        , "to = \"Bob <Bob@example.com>\"\n"
                        , "from = \"Alice <Alice@example.com>\"\n"
                        , "cc = \"Cynthia <Cynthia@example.com>\"\n"
                        , "bcc = \"Dieter <Dieter@example.com>\"\n\n"
                        ]
            mustachedText `shouldBe` Right text
    describe "mustache" $
        it "mustaches the MailTemplate correctly" $ do
            (mustachedTemplate, _, _) <- getMustachedTemplate
            headerFileContent <-
                T.readFile $ fromAbsFile $ mustachedHeaderFile mustachedTemplate
            headerFileContent `shouldBe`
                "subject = \"Reminder email\"\nto = \"Bob <Bob@example.com>\"\nfrom = \"Alice <Alice@example.com>\"\ncc = \"Cynthia <Cynthia@example.com>\"\nbcc = \"Dieter <Dieter@example.com>\"\n\n"
    describe "dealWithMails" $
        it "Can create an email and transform it into Text" $ do
            (mustachedTemplate, heading, orgfile) <- getMustachedTemplate
            mailEither <- createEmail heading orgfile mustachedTemplate
            case mailEither of
                Left errMess -> printErrMess (lines errMess) Error
                Right mail ->
                    mailToString mail `shouldBe`
                    unlines
                        [ "Do you want to send the following email?"
                        , "From: Alice <Alice@example.com>"
                        , "To: Bob <Bob@example.com>"
                        , "CC: Cynthia <Cynthia@example.com>"
                        , "BCC: Dieter <Dieter@example.com>"
                        , "Subject: Reminder email"
                        , "text/plain: Dear Bob,\n\nThis is a reminder email for the task a waiting task.\n\nBest regards,\nAlice\n\n"
                        , "text/html: "
                        ]
