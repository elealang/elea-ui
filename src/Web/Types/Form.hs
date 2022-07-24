--
-- WEB: Types
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types.Form (
    Form (..)
  , Field (..)
  , FieldType (..)
  ) where


import Data.Aeson (
    ToJSON (..)
  , object, (.=)
  )
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import qualified Data.Text as T (empty)
import GHC.Generics


-- | Form
data Form = Form {
  fields     :: [Field]
}

instance ToJSON Form where
  toJSON (Form fields) = 
    let rec field = (fromText field.markupId) .= T.empty
    in  object (rec <$> fields)


-- | Field
data Field = Field {
    label         :: Text
  , defaultValue  :: Maybe Text
  , fieldType     :: FieldType
  , markupId      :: Text
  , isModel       :: Bool
  , valueFunction :: Maybe Text
}


-- | Field Type
data FieldType = 
    FieldTypeText
  | FieldTypeParagraph
  | FieldTypeAppId

instance Show FieldType where
  show FieldTypeText = "text"
  
