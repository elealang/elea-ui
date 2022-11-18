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

import           Elea.Object.Program (Program, ProgramState)


data Page = Page {
    assets          :: Assets
  , program         :: Program
  , programState    :: ProgramState 
  , contextPaneOpen :: Bool
}

new :: Assets -> Program -> ProgramState -> Page
new assets_ prog progState = Page {
    assets          = assets_
  , program         = prog
  , programState    = progState
  , contextPaneOpen = True

}

instance Component Page PageComponentData where
  toComponentData page = PageComponentData {
      contextPaneOpen = page.contextPaneOpen
  }


newtype PageComponentData = PageComponentData {
    contextPaneOpen :: Bool
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

