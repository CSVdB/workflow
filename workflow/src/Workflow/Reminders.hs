{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Workflow.Reminders where

import Data.ByteString.Lazy as LBS hiding (concat)
import Data.HashMap.Strict
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.LocalTime
import Import hiding (lookup)
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
    timezone <- getCurrentTimeZone
    currentLocalTime <- zonedTimeToLocalTime <$> getZonedTime
    case ageOfTask timezone currentLocalTime (heading, orgfile) of
        Left errMess -> printErrMess (lines errMess) shouldPrint
        Right daysAgo ->
            case headingReceiver headingProperties of
                Nothing -> pure ()
                Just toEmailAddress ->
                    when (daysAgo < maxDays) $ do
                        let mail =
                                createEmail
                                    (heading, orgfile)
                                    toEmailAddress
                                    fromAddress
                        print $ mailToText mail
                        shouldSendReminder <- askToSendReminder
                        when shouldSendReminder $ sendReminder mail

sendReminder :: Mail -> IO ()
sendReminder = renderSendMail

-- https://hackage.haskell.org/package/mime-mail-0.4.11/docs/Network-Mail-Mime.html#t:Mail
ageOfTask :: TimeZone
          -> LocalTime
          -> (Heading, Path Rel File)
          -> Either String Int
ageOfTask timezone currentLocalTime (heading@Heading {..}, orgfile) =
    let maybeDate = getDate heading
    in case maybeDate of
           Nothing ->
               Left $
               "The following waiting-task has no date:" ++
               " " ++ fromRelFile orgfile ++ " " ++ T.unpack title
           Just localTimeDate ->
               Right $ getDaysDifference timezone currentLocalTime localTimeDate

data HeadingProperties = HeadingProperties
    { headingReceiver :: Maybe Address
    , headingMaxDays :: Maybe Int
    } deriving (Show, Eq)

getHeadingPropertiesFromHeading :: Heading -> HeadingProperties
getHeadingPropertiesFromHeading Heading {..} =
    let hashMap = sectionProperties section
        receiver = Address Nothing <$> lookup "emailAddress" hashMap
        maxDays = join $ maybeRead . T.unpack <$> lookup "maxDays" hashMap
    in HeadingProperties receiver maxDays

createEmail :: (Heading, Path Rel File) -> Address -> Address -> Mail
createEmail _ _ _ = undefined

-- Use simpleMail'
-- https://hackage.haskell.org/package/mime-mail-0.4.11/docs/Network-Mail-Mime.html#t:Address
mailToText :: Mail -> Text
mailToText mail =
    T.unlines $
    [ "Do you want to send the following email?"
    , T.append "From: " $ addressEmail (mailFrom mail)
    , T.append "To: " $ T.unwords (addressEmail <$> mailTo mail)
    , T.append "Subject: " $ T.concat (snd <$> mailHeaders mail)
    ] ++
    fmap partToText (concat $ mailParts mail)

partToText :: Part -> Text
partToText Part {..} = decodeUtf8 $ toStrict partContent

askToSendReminder :: IO Bool
askToSendReminder = undefined
