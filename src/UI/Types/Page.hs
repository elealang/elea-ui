--
-- WEB: Types.Page
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module UI.Types.Page (
    Page (..)
  , PageHeader (..)
  , PageElea (..)
  ) where


import           Data.Aeson (
    ToJSON (..)
  , object, (.=)
  )
import           Data.Text (Text)
import           Text.Blaze.Html (Html)

import           UI.Config (EleaConfig)
import           UI.Data.Assets (Assets (..))
import           UI.Types.Elea (EleaState)


data Page = Page {
    assets              :: Assets
  , storyPaneOpen       :: Bool
  , modalPaneOpen       :: Bool
  , showStoryButton     :: Bool
  , currentStateVerb    :: Text
  , currentStateObject  :: Text
  , previousStateVerb   :: Maybe Text
  , previousStateObject :: Maybe Text
  , header              :: PageHeader
  , contentHTML         :: Html
  , sidebarHTML         :: Html
}

instance ToJSON Page where
  toJSON page = object [
      "storyPaneOpen"         .= page.storyPaneOpen
    , "showStoryButton"       .= page.showStoryButton
    , "modal_pane_open"       .= page.modalPaneOpen
    , "current_state_verb"    .= page.currentStateVerb
    , "current_state_object"  .= page.currentStateObject
    , "previous_state_verb"   .= page.previousStateVerb
    , "previous_state_object" .= page.previousStateObject
    ]


data PageHeader = PageHeader {
    leftHTML   :: Html
  , centerHTML :: Html
  , rightHTML  :: Html
}


data PageElea = PageElea {
    state             :: EleaState
  , config            :: EleaConfig
  , showEditorSidebar :: Bool
}

instance ToJSON PageElea where
  toJSON page = object [
      "showEditorSidebar" .= page.showEditorSidebar
    ]

