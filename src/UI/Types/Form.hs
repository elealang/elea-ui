--
-- WEB: Types
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module UI.Types.Form (
  -- Types
    Form (..)
  --   Fields
  , Field (..)
  , FieldBasic (..), FieldChoice (..)
  , FieldType (..)
  -- Constructors
  , textField, textFieldWithDefault
  , paragraphField, paragraphFieldWithDefault
  , divider
  ) where


import Data.Aeson (
    ToJSON (..)
  , object, (.=)
  )
import Data.Aeson.Key (fromText)
import qualified Data.List as L (concat)
import Data.Text (Text)
import qualified Data.Text as T (empty)
import GHC.Generics


-- | Form
data Form = Form {
  fields     :: [Field]
}

instance ToJSON Form where
  toJSON (Form fields) = 
    let rec (Basic  field                  ) = 
          [(fromText field.markupId) .= T.empty]
        rec (Choice (FieldChoice _ choices)) =
          (\c -> (fromText $ fst c) .= T.empty) <$> choices
        rec (Line   fields                 ) =
          fields >>= rec
    in  object $ L.concat $ (rec <$> fields)


-- | Field
data Field = 
    Basic FieldBasic
  | Choice FieldChoice
  | Line [Field]


data FieldBasic = FieldBasic {
    label         :: Text
  , defaultValue  :: Maybe Text
  , fieldType     :: FieldType
  , markupId      :: Text
  , isModel       :: Bool
  , valueFunction :: Maybe Text
}


textField :: Text -> Text -> FieldBasic
textField label markupId = FieldBasic {
    label         = label
  , defaultValue  = Nothing
  , fieldType     = FieldTypeText
  , markupId      = markupId
  , isModel       = False
  , valueFunction = Nothing
}

textFieldWithDefault :: Text -> Text -> Text -> FieldBasic
textFieldWithDefault label markupId defaultValue = FieldBasic {
    label         = label
  , defaultValue  = Just defaultValue
  , fieldType     = FieldTypeText
  , markupId      = markupId
  , isModel       = False
  , valueFunction = Nothing
}


paragraphField :: Text -> Text -> FieldBasic
paragraphField label markupId = FieldBasic {
    label         = label
  , defaultValue  = Nothing
  , fieldType     = FieldTypeParagraph
  , markupId      = markupId
  , isModel       = False
  , valueFunction = Nothing
}

paragraphFieldWithDefault :: Text -> Text -> Text -> FieldBasic
paragraphFieldWithDefault label markupId defaultValue = FieldBasic {
    label         = label
  , defaultValue  = Just defaultValue
  , fieldType     = FieldTypeParagraph
  , markupId      = markupId
  , isModel       = False
  , valueFunction = Nothing
}

-- | Divider "field"
divider :: Field
divider = Basic $ FieldBasic {
    label         = ""
  , defaultValue  = Nothing
  , fieldType     = FieldTypeDivider
  , markupId      = ""
  , isModel       = False
  , valueFunction = Nothing
}


data FieldChoice = FieldChoice {
    label   :: Text
  , choices :: [(Text, FieldBasic)]
}


-- | Field Type
data FieldType = 
    FieldTypeText
  | FieldTypeParagraph
  | FieldTypeAppId
  | FieldTypeDivider

instance Show FieldType where
  show FieldTypeText = "text"
  
