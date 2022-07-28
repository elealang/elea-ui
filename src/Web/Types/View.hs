--
-- WEB: Types.View
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types.View (
    -- Public types
    View (..)
    -- Constructors
  , new
  ) where


import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  )
import Data.Text (Text)
import GHC.Generics

import Web.Types.State (State)


-- | View
data View = View {
    chooserOpen :: Bool
  , state       :: State
} deriving (Generic, Show)

instance ToJSON View where
  toEncoding = genericToEncoding defaultOptions


new :: State -> View
new state = View False state


-- | Get the "verb" for a view type
--verb :: ViewType -> Text
--verb DefineStory   = "define"
--verb DefineProgram = "define"
--verb DefineArrow   = "define"

-- | Get the "object" for a view type
--object :: ViewType -> Text
--object DefineStory   = "story"
--object DefineProgram = "program"
--object DefineArrow   = "arrow"


-- | Get the "object" for a view type
--next :: ViewType -> [ViewType]
--next DefineStory = [DefineProgram]
--next _           = []
