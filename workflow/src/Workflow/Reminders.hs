{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Workflow.Reminders where

import Import

import Control.Arrow

import qualified Data.ByteString.Lazy as LB
import qualified Data.Configurator as C
import Data.Either.Utils hiding (fromRight)
import qualified Data.HashMap.Strict as HM
import Data.OrgMode.Parse
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import Data.Time.LocalTime

import Network.CGI.Protocol
import Network.Mail.Mime

import Text.Email.Validate
import Text.Mustache
import Text.Mustache.Types
import Text.Parsec.Error

import Workflow.OptParse
import Workflow.Utils
import Workflow.Waiting

reminders :: RemSets -> Settings -> IO ()
reminders RemSets {..} settings = do
    (headings, errMess) <- getWaitingHeadings setsWorkDir settings
    printErrMess errMess setsShouldPrint
    zonedTime <- getZonedTime
    mapM_
        (sendReminderIfNeeded
             setsShouldPrint
             setsMaxDays
             setsFromAddress
             setsTemplateFiles
             zonedTime)
        headings

withWarningEither :: ShouldPrint -> Either String a -> (a -> IO ()) -> IO ()
withWarningEither shouldPrint (Left err) _ = printErrMess [err] shouldPrint
withWarningEither _ (Right x) func = func x

withWarningEitherStrings :: ShouldPrint
                         -> Either [String] a
                         -> (a -> IO ())
                         -> IO ()
withWarningEitherStrings shouldPrint (Left errs) =
    withWarningEither shouldPrint (Left $ unlines errs)
withWarningEitherStrings shouldPrint (Right x) =
    withWarningEither shouldPrint (Right x)

withWarningMaybe :: ShouldPrint -> Maybe a -> String -> (a -> IO ()) -> IO ()
withWarningMaybe shouldPrint Nothing s _ = printErrMess [s] shouldPrint
withWarningMaybe _ (Just x) _ f = f x

longEnough :: Int
           -> HeadingProperties
           -> Heading
           -> Path Rel File
           -> ZonedTime
           -> Bool
longEnough globalMaxDays headingProperties heading orgfile zonedTime =
    let maxDays = fromMaybe globalMaxDays $ headingMaxDays headingProperties
    in case ageOfTaskInDays zonedTime (heading, orgfile) of
           Left _ -> False
           Right daysAgo -> daysAgo >= maxDays

useJustOrAlternative :: Maybe a -> b -> (a -> IO b) -> IO b
useJustOrAlternative Nothing x _ = pure x
useJustOrAlternative (Just x) _ func = func x

useRightOrAlternative :: Either a b -> (a -> IO c) -> (b -> IO c) -> IO c
useRightOrAlternative (Left x) r _ = r x
useRightOrAlternative (Right x) _ f = f x

fromRight :: b -> Either a b -> b
fromRight x (Left _) = x
fromRight _ (Right x) = x

getTemplateFiles :: HeadingProperties -> TemplateFiles -> IO TemplateFiles
getTemplateFiles headingProperties templateFiles =
    useJustOrAlternative (headingTemplateFile headingProperties) templateFiles $ \filePath -> do
        files <- getExistingPathsFromTemplateFile filePath
        pure $ fromRight templateFiles $ templateToTemplateFiles files

sendReminderIfNeeded
    :: ShouldPrint
    -> Int
    -> Address
    -> TemplateFiles
    -> ZonedTime
    -> (Heading, Path Rel File)
    -> IO ()
sendReminderIfNeeded shouldPrint globalMaxDays fromAddress globalTemplateFiles zonedTime (heading@Heading {..}, orgfile) = do
    let headingProperties = getHeadingProperties heading
    when (longEnough globalMaxDays headingProperties heading orgfile zonedTime) $ do
        templateFiles <- getTemplateFiles headingProperties globalTemplateFiles
        let subject = "Reminder email"
        withWarningMaybe
            shouldPrint
            (getMailInfo fromAddress subject headingProperties)
            ("Mustaching " ++ fromRelFile orgfile ++ " failed") $ \mailInfo -> do
            mustachedTextsEither <- getMustachedTexts mailInfo templateFiles
            withWarningEitherStrings shouldPrint mustachedTextsEither $ \mustachedText -> do
                mailEither <- createEmail heading orgfile mustachedText
                withWarningEither shouldPrint mailEither $ \mail -> do
                    putStr $ mailToString mail
                    shouldSendReminder <-
                        question No "Do you want to send this email?"
                    when (shouldSendReminder == Yes) $ renderSendMail mail

getMailInfo :: Address -> Text -> HeadingProperties -> Maybe MailInfo
getMailInfo fromAddress subject HeadingProperties {..} =
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
        cc = fromMaybe "" . msum $ map (`HM.lookup` hashMap) propertyCc
        bcc = fromMaybe "" . msum $ map (`HM.lookup` hashMap) propertyBcc
    in HeadingProperties receivers cc bcc maxDays templateFile title

getMustachedTexts :: MailInfo
                  -> TemplateFiles
                  -> IO (Either [String] MustachedTexts)
getMustachedTexts mailInfo TemplateFiles {..} = do
    mustacheHeaderEither <- mustacheFile mailInfo headerFile
    useRightOrAlternative mustacheHeaderEither (pure . Left) $ \mustacheHeader -> do
        newHeaderFile <-
            resolveFile (parent headerFile) $ "temp" ++ fileExtension headerFile
        T.writeFile (fromAbsFile newHeaderFile) mustacheHeader
        bodyEither <- mustacheFile mailInfo bodyFile
        useRightOrAlternative bodyEither (pure . Left) $ \body -> do
            bodyFileExt <-
                case getExtension bodyFile of
                    Just x -> pure x
                    Nothing ->
                        die $
                        fromAbsFile bodyFile ++
                        " should have extension .txt or .html, but hasn't!"
            case altBodyFile of
                Nothing ->
                    pure . Right $
                    MustachedTexts newHeaderFile (bodyFileExt, body) Nothing
                Just bodyFile2 -> do
                    mustacheAltBodyEither <- mustacheFile mailInfo bodyFile2
                    useRightOrAlternative mustacheAltBodyEither (pure . Left) $
                        pure .
                        Right .
                        MustachedTexts newHeaderFile (bodyFileExt, body) . Just

getExtension :: Path a File -> Maybe Extension
getExtension path =
    case fileExtension path of
        ".txt" -> Just Plain
        ".html" -> Just Html
        _ -> Nothing

mustacheFile :: MailInfo -> Path Abs File -> IO (Either [String] Text)
mustacheFile mailInfo file = do
    templateEither <- getTemplate file
    useRightOrAlternative templateEither (pure . Left) $ \template ->
        let (substErrs, text) = checkedSubstitute template mailInfo
        in if null substErrs
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
        object $
        let sender =
                case parseAddress from of
                    Right (Address (Just name) _) ->
                        ["senderName" ~> String name]
                    _ -> []
            receiver =
                case parseAddresses to of
                    Right (Address (Just name) _:_) ->
                        ["receiverName" ~> String name]
                    _ -> []
        in sender ++
           receiver ++
           [ "to" ~> String to
           , "from" ~> String from
           , "cc" ~> String cc
           , "bcc" ~> String bcc
           , "subject" ~> String subject
           , "taskTitle" ~> String taskTitle
           ]

getBodyFromTemplate :: Extension -> MustachedTexts -> Maybe Text
getBodyFromTemplate extension MustachedTexts {..} =
    if fst body == extension
        then Just $ snd body
        else altBody

removeFileIfExists :: Path Abs File -> IO ()
removeFileIfExists = ignoringAbsence . removeFile

destroyFiles :: TemplateFiles -> IO ()
destroyFiles TemplateFiles {..} = do
    removeFileIfExists headerFile
    _ <- mapM removeFileIfExists altBodyFile
    removeFileIfExists bodyFile

throwErrorIfNothingAnd :: Maybe a
                       -> String
                       -> (a -> Either String b)
                       -> Either String b
throwErrorIfNothingAnd (Just x) _ func = func x
throwErrorIfNothingAnd Nothing errMess _ = Left errMess

createErrMess :: Text -> Text -> Text -> Path Rel File -> String
createErrMess errMess headerItem title orgfile =
    T.unpack $
    T.concat
        [ errMess
        , " for "
        , headerItem
        , " in task "
        , title
        , " in "
        , T.pack $ fromRelFile orgfile
        ]

createEmail :: Heading
            -> Path Rel File
            -> MustachedTexts
            -> IO (Either String Mail)
createEmail Heading {..} orgfile temp@MustachedTexts {..} = do
    let plainBody = LT.fromStrict <$> getBodyFromTemplate Plain temp
    let htmlBody = LT.fromStrict <$> getBodyFromTemplate Html temp
    let mailParts =
            pure <$>
            (plainPart <$> maybeToList plainBody) ++
            (htmlPart <$> maybeToList htmlBody)
    let headerFilePath = fromAbsFile mustachedHeaderFile
    config <- C.load [C.Optional headerFilePath]
    subjectMaybe <- C.lookup config "subject"
    toAddressesEither <- parseAddressesFromMaybes <$> C.lookup config "to"
    fromAddressEither <- parseAddressFromMaybe <$> C.lookup config "from"
    ccAddresses <-
        eitherListToList . parseAddressesFromMaybes <$> C.lookup config "cc"
    bccAddresses <-
        eitherListToList . parseAddressesFromMaybes <$> C.lookup config "bcc"
    let headerItemAbsentErr headerItem =
            "The template " ++ headerFilePath ++ " has no " ++ headerItem
    pure $
        throwErrorIfNothingAnd subjectMaybe (headerItemAbsentErr "subject") $ \subject ->
            case toAddressesEither of
                Left errMess ->
                    Left $ createErrMess errMess "toAddresses" title orgfile
                Right [] -> Left $ headerItemAbsentErr "toAddresses"
                Right toAddresses ->
                    case fromAddressEither of
                        Left "" -> Left $ headerItemAbsentErr "fromAddress"
                        Left errMess ->
                            Left $
                            createErrMess errMess "fromAddress" title orgfile
                        Right fromAddress ->
                            Right $
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

addressToText :: Address -> Text
addressToText Address {..} =
    T.append
        (case addressName of
             Nothing -> ""
             Just name -> T.append name " ") $
    T.concat ["<", addressEmail, ">"]

parseAddresses :: Text -> Either Text [Address]
parseAddresses t =
    let (errMess, addresses) =
            first T.unlines $
            partitionEithers $ parseAddress <$> T.splitOn ", " t
    in case errMess of
           "" -> Right addresses
           _ -> Left errMess

parseAddressesFromMaybes :: Maybe Text -> Either Text [Address]
parseAddressesFromMaybes s =
    case s of
        Nothing -> Right []
        Just text -> parseAddresses text

parseAddressFromMaybe :: Maybe Text -> Either Text Address
parseAddressFromMaybe s = join $ parseAddress <$> maybeToEither "" s

parseAddress :: Text -> Either Text Address
parseAddress text =
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
        '>':reverseMail ->
            decodeUtf8 <$>
            (canonicalizeEmail . encodeUtf8 . T.pack $ reverse reverseMail)
        _ -> Nothing

parseAddressErrMess :: Text
parseAddressErrMess = "Could not parse the address"

templateEitherToTemplate :: Either ParseError Template
                         -> Either [String] Template
templateEitherToTemplate (Left errMess) =
    Left $ messageString <$> errorMessages errMess
templateEitherToTemplate (Right template) = Right template

getTemplate :: Path Abs File -> IO (Either [String] Template)
getTemplate file = do
    templateEither <-
        automaticCompile [fromAbsDir . parent $ file] $
        fromRelFile . filename $ file
    pure $ templateEitherToTemplate templateEither

mailToString :: Mail -> String
mailToString mail@Mail {..} =
    T.unpack . T.unlines $
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

partToText :: Part -> Text
partToText Part {..} = decodeUtf8 $ LB.toStrict partContent
