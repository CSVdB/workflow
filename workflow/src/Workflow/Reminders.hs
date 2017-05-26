{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Workflow.Reminders where

import Control.Arrow
import qualified Data.ByteString.Lazy as LB
import Data.Configurator
import qualified Data.HashMap.Lazy as LHM
import qualified Data.HashMap.Strict as HM
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import Data.Time.LocalTime
import Import hiding (lookup)
import Network.CGI.Protocol
import Network.Mail.Mime
import Text.Mustache
import Text.Mustache.Types
import Text.Parsec.Error
import Workflow.OptParse
import Workflow.Utils
import Workflow.Waiting

reminders
    :: Int
    -> Path Abs Dir
    -> Address
    -> ShouldPrint
    -> MailTemplate
    -> Settings
    -> IO ()
reminders maxDays workDir fromAddress shouldPrint mailTemplate settings = do
    (headings, errMess) <- getWaitingHeadings workDir settings
    printErrMess errMess shouldPrint
    mapM_
        (sendReminderIfNeeded shouldPrint maxDays fromAddress mailTemplate)
        headings

sendReminderIfNeeded
    :: ShouldPrint
    -> Int
    -> Address
    -> MailTemplate
    -> (Heading, Path Rel File)
    -> IO ()
sendReminderIfNeeded shouldPrint globalMaxDays fromAddress globalMailTemplate (heading@Heading {..}, orgfile) = do
    let headingProperties = getHeadingProperties heading
    let maxDays = fromMaybe globalMaxDays $ headingMaxDays headingProperties
    currentZonedTime <- getZonedTime
    case ageOfTaskInDays currentZonedTime (heading, orgfile) of
        Left errMess -> printErrMess (lines errMess) shouldPrint
        Right daysAgo ->
            when (daysAgo >= maxDays) $ do
                template <-
                    case headingTemplateFile headingProperties of
                        Nothing -> pure globalMailTemplate
                        Just filePath -> do
                            files <- resolveStringToFiles filePath
                            pure $
                                case templateToMailTemplate files of
                                    Left _ -> globalMailTemplate
                                    Right x -> x
                let subject = "Reminder email"
                let mustacheMaybe =
                        getMustache fromAddress subject headingProperties
                case mustacheMaybe of
                    Nothing ->
                        printErrMess
                            ["Mustaching " ++ fromRelFile orgfile ++ " failed"]
                            shouldPrint
                    Just mustache -> do
                        mustacheTemplateEither <-
                            mustacheToMailTemplate mustache template
                        case mustacheTemplateEither of
                            Left errMess -> printErrMess errMess shouldPrint
                            Right mustacheTemplate -> do
                                mailEither <-
                                    createEmail heading orgfile mustacheTemplate
                                case mailEither of
                                    Left errMess ->
                                        printErrMess (lines errMess) shouldPrint
                                    Right mail
                                        -- destroyFiles mustacheTemplate
                                     -> do
                                        putStr . T.unpack $ mailToText mail
                                        shouldSendReminder <-
                                            question
                                                No
                                                "Do you want to send this email?"
                                        when (shouldSendReminder == Yes) $
                                            renderSendMail mail

getMustache :: Address -> Text -> HeadingProperties -> Maybe MailInfo
getMustache fromAddress subject HeadingProperties {..} =
    (\to ->
         MailInfo
             to
             (addressToText fromAddress)
             headingCc
             headingBcc
             subject
             headingTaskTitle) <$>
    headingReceivers

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
    { headingReceivers :: Maybe Text
    , headingCc :: Text
    , headingBcc :: Text
    , headingMaxDays :: Maybe Int
    , headingTemplateFile :: Maybe FilePath
    , headingTaskTitle :: Text
    } deriving (Show, Eq)

propertyReceivers :: [Text]
propertyReceivers = ["receiver"]

propertyCc :: [Text]
propertyCc = ["cc"]

propertyBcc :: [Text]
propertyBcc = ["bcc"]

propertyMaxDays :: [Text]
propertyMaxDays = ["maxDays"]

propertyTemplate :: [Text]
propertyTemplate = ["template"]

getHeadingProperties :: Heading -> HeadingProperties
getHeadingProperties Heading {..} =
    let hashMap = sectionProperties section
        maxDays =
            maybeRead =<<
            T.unpack <$> msum (map (`HM.lookup` hashMap) propertyMaxDays)
        templateFile =
            T.unpack <$> msum (map (`HM.lookup` hashMap) propertyTemplate)
        receivers = msum $ map (`HM.lookup` hashMap) propertyReceivers
        cc = maybeTextToText . msum $ map (`HM.lookup` hashMap) propertyCc
        bcc = maybeTextToText . msum $ map (`HM.lookup` hashMap) propertyBcc
    in HeadingProperties receivers cc bcc maxDays templateFile title

maybeTextToText :: Maybe Text -> Text
maybeTextToText Nothing = ""
maybeTextToText (Just text) = text

mustacheToMailTemplate :: MailInfo
                       -> MailTemplate
                       -> IO (Either [String] MustachedMailTemplate)
mustacheToMailTemplate mustache MailTemplate {..} = do
    mustacheHeaderEither <- mustacheFile mustache headerFile
    case mustacheHeaderEither of
        Left errMess -> pure $ Left errMess
        Right mustacheHeader -> do
            newHeaderFile <-
                resolveFile (parent headerFile) $
                "temp" ++ fileExtension headerFile
            T.writeFile (fromAbsFile newHeaderFile) mustacheHeader
            bodyEither <- mustacheFile mustache (file bodyFile)
            case bodyEither of
                Left errMess -> pure $ Left errMess
                Right body ->
                    case altBodyFile of
                        Nothing ->
                            pure . Right $
                            MustachedMailTemplate
                                newHeaderFile
                                (ext bodyFile, body)
                                Nothing
                        Just bodyFile2 -> do
                            mustacheAltBodyEither <-
                                mustacheFile mustache $ file bodyFile2
                            case mustacheAltBodyEither of
                                Left errMess -> pure $ Left errMess
                                Right altBody ->
                                    pure . Right $
                                    MustachedMailTemplate
                                        newHeaderFile
                                        (ext bodyFile, body) $
                                    Just (ext bodyFile2, altBody)

mustacheFile :: MailInfo -> Path Abs File -> IO (Either [String] Text)
mustacheFile mustache file = do
    templateEither <- fileToTemplate file
    case templateEither of
        Left errMess -> pure $ Left errMess
        Right template -> do
            let (substErrs, text) = checkedSubstitute template mustache
            if null substErrs
                then pure $ Right text
                else pure . Left $ show <$> substErrs

data MailInfo = MailInfo
    { to :: Text
    , from :: Text
    , cc :: Text
    , bcc :: Text
    , subject :: Text
    , taskTitle :: Text
    } deriving (Show, Eq)

instance ToMustache MailInfo where
    toMustache MailInfo {..} =
        let hashMap =
                LHM.insert "to" (String to) $
                LHM.insert "from" (String from) $
                LHM.insert "cc" (String cc) $
                LHM.insert "bcc" (String bcc) $
                LHM.insert "subject" (String subject) $
                LHM.singleton "taskTitle" (String taskTitle)
            hashMapWithSender =
                case parseAddress (Just from) of
                    Right (Address (Just name) _) ->
                        LHM.insert "senderName" (String name) hashMap
                    _ -> hashMap
            hashMapWithReceiver =
                case parseAddresses (Just to) of
                    Right (Address (Just name) _:_) ->
                        LHM.insert
                            "receiverName"
                            (String name)
                            hashMapWithSender
                    _ -> hashMapWithSender
        in Object hashMapWithReceiver

getBodyFromTemplate :: Extension -> MustachedMailTemplate -> Maybe Text
getBodyFromTemplate extension MustachedMailTemplate {..} =
    if fst body == extension
        then Just $ snd body
        else case altBody of
                 Nothing -> Nothing
                 Just body2 ->
                     if fst body2 == extension
                         then Just $ snd body2
                         else Nothing

removeFileIfExists :: Path Abs File -> IO ()
removeFileIfExists path = do
    exists <- doesFileExist path
    when exists $ removeFile path

destroyFiles :: MailTemplate -> IO ()
destroyFiles MailTemplate {..} = do
    removeFileIfExists headerFile
    _ <- mapM (removeFileIfExists . file) altBodyFile
    removeFileIfExists $ file bodyFile

createEmail :: Heading
            -> Path Rel File
            -> MustachedMailTemplate
            -> IO (Either String Mail)
createEmail Heading {..} orgfile temp@MustachedMailTemplate {..} = do
    let plainBody = LT.fromStrict <$> getBodyFromTemplate Plain temp
    let htmlBody = LT.fromStrict <$> getBodyFromTemplate Html temp
    let mailParts =
            pure <$>
            (plainPart <$> maybeToList plainBody) ++
            (htmlPart <$> maybeToList htmlBody)
    let headerFilePath = fromAbsFile mustachedHeaderFile
    config <- load [Optional headerFilePath]
    subjectMaybe <- lookup config "subject"
    toAddressesMaybe <- lookup config "to"
    fromAddressMaybe <- lookup config "from"
    ccAddressesMaybe <- lookup config "cc"
    bccAddressesMaybe <- lookup config "bcc"
    case subjectMaybe of
        Nothing ->
            pure . Left $
            "The template " ++ headerFilePath ++ " has no subject!"
        Just subject ->
            case parseAddresses toAddressesMaybe of
                Left errMess ->
                    pure . Left . T.unpack $
                    T.concat
                        [ errMess
                        , " for toAddresses in task "
                        , title
                        , " in "
                        , T.pack $ fromRelFile orgfile
                        ]
                Right [] ->
                    pure . Left $
                    "The template " ++ headerFilePath ++ " has to toAddresses"
                Right toAddresses ->
                    case parseAddress fromAddressMaybe of
                        Left "" ->
                            pure . Left $
                            "The template " ++
                            headerFilePath ++ " has to fromAddress"
                        Left errMess ->
                            pure . Left . T.unpack $
                            T.concat
                                [ errMess
                                , " for fromAddress in task "
                                , title
                                , " in "
                                , T.pack $ fromRelFile orgfile
                                ]
                        Right fromAddress ->
                            let ccAddresses =
                                    eitherListToList $
                                    parseAddresses ccAddressesMaybe
                                bccAddresses =
                                    eitherListToList $
                                    parseAddresses bccAddressesMaybe
                            in pure . Right $
                               Mail
                                   fromAddress
                                   toAddresses
                                   ccAddresses
                                   bccAddresses
                                   [("subject", subject)]
                                   mailParts

eitherListToList :: Either a [b] -> [b]
eitherListToList (Left _) = []
eitherListToList (Right x) = x

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

maybeListToList :: Maybe [a] -> [a]
maybeListToList Nothing = []
maybeListToList (Just list) = list

addressToText :: Address -> Text
addressToText Address {..} =
    T.append
        (case addressName of
             Nothing -> ""
             Just name -> T.append name " ") $
    T.concat ["<", addressEmail, ">"]

addressesToText :: [Address] -> Text
addressesToText addresses = T.intercalate ", " $ fmap addressToText addresses

parseAddresses :: Maybe Text -> Either Text [Address]
parseAddresses s =
    case s of
        Nothing -> Right []
        Just text ->
            let (errMess, addresses) =
                    first T.unlines $
                    partitionEithers $ textToAddress <$> T.splitOn ", " text
            in case errMess of
                   "" -> Right addresses
                   _ -> Left errMess

parseAddress :: Maybe Text -> Either Text Address
parseAddress s =
    case s of
        Nothing -> Left ""
        Just string -> textToAddress string

textToAddress :: Text -> Either Text Address
textToAddress text =
    case T.splitOn " <" text of
        "":[email] ->
            case getMailAddress email of
                Nothing -> Left $ T.concat [parseAddressErrMess, " ", email]
                Just address -> Right $ Address Nothing address
        name:[email] ->
            case getMailAddress email of
                Nothing ->
                    Left $ T.concat [parseAddressErrMess, " ", name, " ", email]
                Just address -> Right $ Address (Just name) address
        list -> Left $ T.concat list

getMailAddress :: Text -> Maybe Text
getMailAddress text =
    case reverse $ T.unpack text of
        '>':reverseMail -> Just . T.pack $ reverse reverseMail
        _ -> Nothing

parseAddressErrMess :: Text
parseAddressErrMess = "Could not parse the address"

templateEitherToTemplate :: Either ParseError Template
                         -> IO (Either [String] Template)
templateEitherToTemplate tempEither =
    case tempEither of
        Left errMess -> pure . Left $ messageString <$> errorMessages errMess
        Right template -> pure $ Right template

fileToTemplate :: Path Abs File -> IO (Either [String] Template)
fileToTemplate file = do
    templateEither <-
        automaticCompile [fromAbsDir . parent $ file] $
        fromRelFile . filename $ file
    templateEitherToTemplate templateEither

mailToText :: Mail -> Text
mailToText mail@Mail {..} =
    T.unlines $
    [ "Do you want to send the following email?"
    , T.concat ["From: ", printAddress mailFrom]
    , T.concat ["To: ", T.concat $ fmap printAddress mailTo]
    ] ++
    [T.append "CC: " (T.concat (fmap printAddress mailCc))] ++
    [T.append "BCC: " (T.concat (fmap printAddress mailBcc))] ++
    [T.append "Subject: " $ T.concat (snd <$> mailHeaders)] ++
    [T.append "text/plain: " $ bodyOfMail "text/plain; charset=utf-8" mail] ++
    [T.append "text/html: " $ bodyOfMail "text/html; charset=utf-8" mail]

printAddress :: Address -> Text
printAddress Address {..} =
    case addressName of
        Nothing -> T.concat ["<", addressEmail, ">"]
        Just name -> T.concat [name, " <", addressEmail, ">"]

bodyOfMail :: Text -> Mail -> Text
bodyOfMail expectedPartType Mail {..} =
    let byteString =
            LB.concat $
            fmap partContent $
            filter (\part -> partType part == expectedPartType) $
            concat mailParts
    in decodeUtf8 $ LB.toStrict byteString

maybeToText
    :: Show a
    => Maybe a -> Text
maybeToText Nothing = ""
maybeToText (Just x) = T.pack $ show x

partToText :: Part -> Text
partToText Part {..} = decodeUtf8 $ LB.toStrict partContent
