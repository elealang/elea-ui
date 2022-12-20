--
-- WEB: Types.Page
--

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module UI.Types.Page (
    Page (..), new
  , PageComponentData
  ) where


import           Data.Aeson (
    ToJSON (..)
  , object, (.=)
  )

import           Data.Text (Text)
import           GHC.Generics
import           Text.Blaze.Html (Html)

import           UI.Data.Assets (Assets (..))
import           UI.Types.Component

import qualified Elea


data Page = Page {
    assets            :: Assets
  , program           :: Elea.Program
  , historyPaneActive :: Bool
  , previewPaneActive :: Bool
  , eleaPaneActive    :: Bool
}

new :: Assets -> Elea.Program -> Page
new assets_ prog = Page {
    assets             = assets_
  , program            = prog
  , historyPaneActive  = False
  , previewPaneActive  = False
  , eleaPaneActive     = False
}

instance Component Page PageComponentData where
  toComponentData page = PageComponentData {
      historyPaneActive = page.historyPaneActive
    , previewPaneActive = page.previewPaneActive
    , eleaPaneActive    = page.eleaPaneActive
  }


data PageComponentData = PageComponentData {
    historyPaneActive :: Bool
  , previewPaneActive :: Bool
  , eleaPaneActive    :: Bool
} deriving (Eq, Generic, Show)

instance ToJSON PageComponentData

--data Page = Page {
    --assets              :: Assets
  --, storyPaneOpen       :: Bool
  --, modalPaneOpen       :: Bool
  --, showStoryButton     :: Bool
  --, currentStateVerb    :: Text
  --, currentStateObject  :: Text
  --, previousStateVerb   :: Maybe Text
  --, previousStateObject :: Maybe Text
  --, header              :: PageHeader
  --, contentHTML         :: Html
  --, sidebarHTML         :: Html
--}

--instance ToJSON Page where
  --toJSON page = object [
      --"storyPaneOpen"         .= page.storyPaneOpen
    --, "showStoryButton"       .= page.showStoryButton
    --, "modal_pane_open"       .= page.modalPaneOpen
    --, "current_state_verb"    .= page.currentStateVerb
    --, "current_state_object"  .= page.currentStateObject
    --, "previous_state_verb"   .= page.previousStateVerb
    --, "previous_state_object" .= page.previousStateObject
    --]

