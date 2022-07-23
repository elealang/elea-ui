--
-- WEB: Types
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types.Form (
    Form (..)
  , Field (..)
  , FieldType (..)
  ) where


import Data.Text (Text)


data Form = Form {
    fields :: [Field]
}


data Field = Field {
    label        :: Text
  , defaultValue :: Maybe Text
  , fieldType    :: FieldType
  , markupId     :: Text
}


data FieldType = 
    FieldTypeText
  | FieldTypeParagraph

instance Show FieldType where
  show FieldTypeText = "text"
  
