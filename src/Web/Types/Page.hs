--
-- WEB: Types.Page
--

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types.Page (
    Page (..), new
  ) where

import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  )
import GHC.Generics

import Data.Text (Text)


data Page = Page {
  historyPaneOpen :: Bool
} deriving (Generic, Show)

instance ToJSON Page where
  toEncoding = genericToEncoding defaultOptions


new :: Page
new = Page {
    historyPaneOpen = False
}
