--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Page.Elea (
    html
  ) where


import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (toUpper)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Alpine as X
import qualified UI.HTML.Comp.ComputerChooser as ComputerChooser
import qualified UI.HTML.Comp.Home as Home
import qualified UI.HTML.Comp.SetBuilder as SetBuilder
import qualified UI.HTML.Comp.StateButton as StateButton
import qualified UI.HTML.Comp.StoryEditor as StoryEditor
import qualified UI.HTML.Comp.StoryHistory as StoryHistory (html)
import qualified UI.HTML.Comp.ViewSwitcher as ViewSwitcher
import qualified UI.HTML.Page as Page (html)
import qualified UI.Types.ComputerChooser as ComputerChooser
import           UI.Types.Page (
    Page (..)
  , PageHeader (..)
  , PageElea (..)
  )
import qualified UI.Types.Elea as Elea

import           Elea.Base (Def (..))
import           Elea.Server (Server)
import qualified Elea.Set as Set (SetKind (..))


-- | View HTML
html :: PageElea -> Assets -> Html
html page _assets =  Page.html "" page.state Page {
      assets              = _assets
    , storyPaneOpen       = True
    , modalPaneOpen       = False
    , showStoryButton     = True
    , currentStateVerb    = T.toUpper $ Elea.stateVerb page.state
    , currentStateObject  = T.toUpper $ Elea.stateObject page.state
    , previousStateVerb   = Nothing
    , previousStateObject = Nothing
    , header          = PageHeader {
          leftHTML   = headerStoryHTML _assets
        , centerHTML = headerEleaHTML
        , rightHTML  = ViewSwitcher.html _assets
      }
    , contentHTML     = pageHTML page _assets
    , sidebarHTML     = StoryHistory.html Nothing _assets
  }


-- | Page HTMl
pageHTML :: PageElea -> Assets -> Html
pageHTML page _assets = do
  H.div ! A.class_ "page-elea" $ do
    defineHTML page.state _assets
    computeHTML page.state page.config.servers _assets



headerStoryHTML :: Assets -> Html
headerStoryHTML _assets = do
  H.div ! A.class_ "page-elea-header-story" 
        ! H.customAttribute "x-data" "{ show_button: false }"
        ! X.onEnter "show_button = true"
        ! X.onLeave "show_button = false" $ do
    H.div ! A.class_ "page-elea-header-story-name"
          ! X.show_ "!show_button" $
      "Elea Dev Story"
    H.div ! A.class_ "page-elea-header-state-button"
          ! X.show_ "show_button" $
      StateButton.html StateButton.Inline Elea.IndexFindStory _assets


headerEleaHTML :: Html
headerEleaHTML = do
  H.a ! A.class_ "page-elea-header-title" 
      ! A.href "/" $ do
    H.div ! A.class_ "page-elea-header-title-name" $ "ELEA"
    H.div ! A.class_ "page-elea-header-title-description" $ "ENGINE FOR ETHICAL CHANGE"


-- | Define HTML
defineHTML :: Elea.EleaState -> Assets -> Html
defineHTML st a = do
  H.div ! A.class_ "page-elea-define" $ case st of
    Elea.GeneralHome     -> Home.html a
    Elea.EleaCreateStory -> defineSectionHTML (StoryEditor.html a) a
    Elea.IndexFindStory  -> do
      defineSectionHTML (SetBuilder.html Set.Servers a) a
      defineSectionHTML (SetBuilder.html Set.Stories a) a
      defineSectionHTML (SetBuilder.html Set.Computers a) a
    _                    -> return ()


-- | Define section HTML
defineSectionHTML :: Html -> Assets -> Html
defineSectionHTML compHTML a = do
  H.div ! A.class_ "page-elea-define-section" $ do
    editorSidebarHTML "definition" a
    compHTML 


-- | Compute HTML
computeHTML :: Elea.EleaState -> [Server] -> Assets -> Html
computeHTML st servers a = do
  H.div ! A.class_ "page-elea-compute" $ case st of
    Elea.GeneralHome -> return ()
    _                -> computeSectionHTML st servers a

computeSectionHTML :: Elea.EleaState -> [Server] -> Assets -> Html
computeSectionHTML st servers a = do
  H.div ! A.class_ "page-elea-compute-section" $ do
    editorSidebarHTML "computation" a
    case st of
      Elea.EleaCreateStory -> ComputerChooser.html (ComputerChooser.new servers DefStory) a
      _                    -> return ()


-- | Sidebar HTML
editorSidebarHTML :: Text -> Assets -> Html
editorSidebarHTML iconName _assets = do
  H.div ! A.class_ "page-elea-editor-sidebar" $ 
    H.div ! A.class_ "page-elea-editor-sidebar-icon" $
      H.preEscapedText $ fromJust $ iconSVGWithName iconName _assets.iconIndex

