--
-- WEB: Types.View
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types.View (
    -- Public types
    View (..), ViewType (..)
    -- Constructors
  , new
    -- Utility functions
  , verb, object
  , next
  ) where

import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  )
import Data.Text (Text)
import GHC.Generics


-- | View
data View = View {
    chooserOpen :: Bool
  , typ         :: ViewType
} deriving (Generic, Show)

instance ToJSON View where
  toEncoding = genericToEncoding defaultOptions


-- | View Type
data ViewType =
    DefineStory
  | DefineProgram
  deriving (Generic, Show)

instance ToJSON ViewType where
  toEncoding = genericToEncoding defaultOptions


new :: ViewType -> View
new viewType = View False viewType


-- | Get the "verb" for a view type
verb :: ViewType -> Text
verb DefineStory   = "define"
verb DefineProgram = "define"

-- | Get the "object" for a view type
object :: ViewType -> Text
object DefineStory   = "story"
object DefineProgram = "program"


-- | Get the "object" for a view type
next :: ViewType -> [ViewType]
next DefineStory   = [DefineProgram]
next DefineProgram = []
