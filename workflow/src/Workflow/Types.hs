module Workflow.Types where

import Data.Configurator.Types
import qualified Data.Text as T
import Data.Text (Text)
import Import

data ShouldPrint
    = Error
    | Warning
    | Not
    deriving (Show, Eq)

data Extension
    = Plain
    | Html
    deriving (Show, Eq)

data TemplateFiles = TemplateFiles
    { headerFile :: Path Abs File
    , bodyFile :: Path Abs File
    , altBodyFile :: Maybe (Path Abs File)
    } deriving (Show, Eq)

data MustachedTexts = MustachedTexts
    { mustachedHeaderFile :: Path Abs File
    , body :: (Extension, Text)
    , altBody :: Maybe Text
    } deriving (Show, Eq)

parseShouldPrint :: String -> Maybe ShouldPrint
parseShouldPrint "error" = Just Error
parseShouldPrint "nothing" = Just Not
parseShouldPrint "warning" = Just Warning
parseShouldPrint _ = Nothing

instance Configured ShouldPrint where
    convert (String string) = parseShouldPrint $ T.unpack string
    convert _ = Nothing
