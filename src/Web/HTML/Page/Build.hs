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
import           Config (EleaConfig (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.Comp.ArrowEditor as ArrowEditor
import qualified Web.HTML.Comp.ComputerChooser as ComputerChooser
import qualified Web.HTML.Comp.ProgramEditor as ProgramEditor
import qualified Web.HTML.Comp.StoryEditor as StoryEditor
import qualified Web.HTML.Comp.ViewSwitcher as ViewSwitcher
import qualified Web.HTML.Page as Page (html)
import qualified Web.Types.ComputerChooser as ComputerChooser
import           Web.Types.Page (Page (..))
import           Web.Types.State (State)
import qualified Web.Types.State as State

import           Elea.Base (Object (..))
import           Elea.Index as ComputerIndex (fromComputers)


-- | View HTML
html :: EleaConfig -> Assets -> State -> Html
html config assets state =  Page.html page
  where
    headerHTML  = ViewSwitcher.html state assets
    page        = Page {
        assets           = assets
      , storyPaneOpen    = False
      , showStoryButton  = True
      , headerCenterHTML = headerHTML
      , contentHTML      = pageHTML config state assets
    }


-- | Page HTMl
pageHTML :: EleaConfig -> State -> Assets -> Html
pageHTML config state assets = do
  H.div ! A.class_ "page-build" $ do
    H.div ! A.class_ "page-build-main" $ do
      sidebarHTML "edit" assets
      mainHTML
    H.div ! A.class_ "page-build-next" $ do
      sidebarHTML "arrow-right" assets
      nextHTML
  where
    compIndex = ComputerIndex.fromComputers config.computers
    mainHTML = case state of
      State.MainCreateStory -> StoryEditor.html assets
      _ -> return ()
      --DefineProgram -> ProgramEditor.html assets
      --DefineArrow   -> ArrowEditor.html assets
    nextHTML = case state of
      State.MainCreateStory -> ComputerChooser.html (ComputerChooser.new compIndex ObjectStory) assets
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

