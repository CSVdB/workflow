{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Workflow.Reminders where

import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.LocalTime
import Import
import Network.CGI.Protocol
import Network.Mail.Mime
import Workflow.OptParse
import Workflow.Utils
import Workflow.Waiting

reminders :: Int -> Path Abs Dir -> Address -> ShouldPrint -> Settings -> IO ()
reminders maxDays workDir fromAddress shouldPrint settings = do
    (headings, errMess) <- getWaitingHeadings workDir settings
    printErrMess errMess shouldPrint
    mapM_ (sendReminderIfNeeded shouldPrint maxDays fromAddress) headings

sendReminderIfNeeded :: ShouldPrint
                     -> Int
                     -> Address
                     -> (Heading, Path Rel File)
                     -> IO ()
sendReminderIfNeeded shouldPrint globalMaxDays fromAddress (heading@Heading {..}, orgfile) = do
    let headingProperties = getHeadingPropertiesFromHeading heading
    let maxDays = fromMaybe globalMaxDays $ headingMaxDays headingProperties
    currentZonedTime <- getZonedTime
    case ageOfTaskInDays currentZonedTime (heading, orgfile) of
        Left errMess -> printErrMess (lines errMess) shouldPrint
        Right daysAgo ->
            case headingReceiver headingProperties of
                Nothing -> pure ()
                Just toAddress ->
                    when (daysAgo >= maxDays) $ do
                        let mail = createEmail heading fromAddress toAddress
                        putStr . T.unpack $ mailToText mail
                        shouldSendReminder <-
                            question No "Do you want to send this email?"
                        when (shouldSendReminder == Yes) $ renderSendMail mail

ageOfTaskInDays :: ZonedTime -> (Heading, Path Rel File) -> Either String Int
ageOfTaskInDays zonedTime (heading@Heading {..}, orgfile) =
    let maybeDate = getDate heading
    in case maybeDate of
           Nothing ->
               Left $
               "The following waiting-task has no date:" ++
               " \"" ++ T.unpack title ++ "\" in " ++ fromRelFile orgfile
           Just localTimeDate ->
               Right $ getDaysDifference zonedTime localTimeDate

data HeadingProperties = HeadingProperties
    { headingReceiver :: Maybe Address
    , headingMaxDays :: Maybe Int
    } deriving (Show, Eq)

propertyEmailAddressNames :: [Text]
propertyEmailAddressNames = ["emailAddress", "email"]

propertyMaxDaysName :: Text
propertyMaxDaysName = "maxDays"

getHeadingPropertiesFromHeading :: Heading -> HeadingProperties
getHeadingPropertiesFromHeading Heading {..} =
    let hashMap = sectionProperties section
        receiver =
            Address Nothing <$>
            msum (map (`HM.lookup` hashMap) propertyEmailAddressNames)
        maxDays =
            join $
            maybeRead . T.unpack <$> HM.lookup propertyMaxDaysName hashMap
    in HeadingProperties receiver maxDays

createEmail :: Heading -> Address -> Address -> Mail
createEmail Heading {..} fromAddress toAddress =
    let subject = "reminder"
        body = "This is a reminder email."
    in simpleMail' toAddress fromAddress subject body

mailToText :: Mail -> Text
mailToText mail =
    T.unlines $
    [ "Do you want to send the following email?"
    , T.append "From: " $ addressEmail (mailFrom mail)
    , T.append "To: " $ T.unwords (addressEmail <$> mailTo mail)
    , T.append "Subject: " $ T.concat (snd <$> mailHeaders mail)
    , "Body:"
    ] ++
    fmap partToText (concat $ mailParts mail)

partToText :: Part -> Text
partToText Part {..} = decodeUtf8 $ LB.toStrict partContent
