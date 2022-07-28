--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Page.Build (
    html
  ) where


import           Data.Maybe (fromJust)
import           Data.Text (Text)

import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.Comp.ArrowEditor as ArrowEditor
import qualified Web.HTML.Comp.ComputerChooser as ComputerChooser
import qualified Web.HTML.Comp.ProgramEditor as ProgramEditor
import qualified Web.HTML.Comp.StoryEditor as StoryEditor
import qualified Web.HTML.Comp.ViewSwitcher as ViewSwitcher
import qualified Web.HTML.Page as Page (html)
import           Web.Types.ComputerChooser (EntityType (..))
import qualified Web.Types.ComputerChooser as ComputerChooser
import           Web.Types.Page (Page (..))
import           Web.Types.State (State)
import qualified Web.Types.State as State


-- | View HTML
html :: Assets -> State -> Html
html assets state =  Page.html page
  where
    headerHTML  = ViewSwitcher.html state assets
    page        = Page {
        assets           = assets
      , storyPaneOpen    = False
      , showStoryButton  = True
      , headerCenterHTML = headerHTML
      , contentHTML      = pageHTML state assets
    }


-- | Page HTMl
pageHTML :: State -> Assets -> Html
pageHTML state assets = do
  H.div ! A.class_ "page-build" $ do
    H.div ! A.class_ "page-build-main" $ do
      sidebarHTML "edit" assets
      mainHTML
    H.div ! A.class_ "page-build-next" $ do
      sidebarHTML "arrow-right" assets
      nextHTML
  where
    mainHTML = case state of
      State.MainCreateStory -> StoryEditor.html assets
      _ -> return ()
      --DefineProgram -> ProgramEditor.html assets
      --DefineArrow   -> ArrowEditor.html assets
    nextHTML = case state of
      State.MainCreateStory -> ComputerChooser.html (ComputerChooser.new Story) assets
      _ -> return ()
      --DefineStory   -> 
      --DefineProgram -> ComputerChooser.html (ComputerChooser.new Program) assets
      --DefineArrow   -> ComputerChooser.html (ComputerChooser.new Arrow) assets


-- | Sidebar HTML
sidebarHTML :: Text -> Assets -> Html
sidebarHTML iconName (Assets iconIndex) = do
  H.div ! A.class_ "page-build-sidebar" $
    H.div ! A.class_ "page-build-sidebar-icon" $
      H.preEscapedText $ fromJust $ iconSVGWithName iconName iconIndex

