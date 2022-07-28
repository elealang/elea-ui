--
-- WEB: Types.Page
--

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types.Page (
    Page (..)
  ) where


import           Data.Aeson (
    ToJSON (..)
  , object, (.=)
  )
import           GHC.Generics
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Data.Text (Text)
import qualified Data.Text as T (empty)


data Page = Page {
    assets           :: Assets
  , storyPaneOpen    :: Bool
  , showStoryButton  :: Bool
  , headerCenterHTML :: Html
  , contentHTML      :: Html
}

instance ToJSON Page where
  toJSON page = object $ [
      "storyPaneOpen"   .= page.storyPaneOpen
    , "showStoryButton" .= page.showStoryButton
    ]


