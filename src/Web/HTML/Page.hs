--
-- HTML: Page
--

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.HTML.Page (
    documentHTML
  ) where


import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze (customAttribute, preEscapedText)
import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)
import Web.HTML.Comp.ViewSwitcher as ViewSwitcher
import qualified Web.HTML.Alpine as X
import Web.Types.Page (Page)
import qualified Web.Types.Page as Page
import qualified Web.Types.View as View


-- | Document HTML
documentHTML :: Assets -> Html -> Html
documentHTML assets contentHtml = H.docTypeHtml $ do
  H.head $ do
    H.title "Elea"
    -- Fonts
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "https://cloud.typography.com/6602898/6169632/css/fonts.css"
    -- CSS: Vars
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/vars.css"
    -- CSS: Page
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/page.css"
    -- CSS/Component: AbstractionManager
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/abstraction-manager.css"
    -- CSS/Component: Form
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-form.css"
    -- CSS/Component: ComputerChooser
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/computer-chooser.css"
    -- CSS/Component: ProgramManager
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/program-manager.css"
    -- CSS/Component: ProgramEditor
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-program-editor.css"
    -- CSS/Component: StoryEditor
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-story-editor.css"
    -- CSS/Component: ViewSwitcher
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-view-switcher.css"
    -- CSS/Component: View
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-view.css"
    -- Alpine
    H.preEscapedString "<script defer src='https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js'></script>"
  H.body $ pageHTML Page.new contentHtml assets


pageHTML :: Page -> Html -> Assets -> Html
pageHTML page contentHtml assets = do
  X.html page "page" $ do
    pageHeaderHTML assets
    H.div ! A.class_ "page-main" $ do
      pageSidebarHTML assets
      H.div ! A.class_ "page-content" $ contentHtml
    pageFooterHTML assets
 

-- | Page header HTML
pageHeaderHTML :: Assets -> Html
pageHeaderHTML assets = do 
  H.div ! A.class_ "page-header" $ do
    H.div ! A.class_ "page-title" $ do
      H.div ! A.class_ "page-title-name" $ "ELEA"
      H.div ! A.class_ "page-title-description" $ "ENGINE FOR ETHICAL CHANGE"
    ViewSwitcher.html (View.new View.DefineStory) assets
    H.div ! A.class_ "page-account" $ return ()


-- | Page header HTML
pageSidebarHTML :: Assets -> Html
pageSidebarHTML assets = do
  H.div ! A.class_ "page-sidebar" 
        ! customAttribute "x-show" "historyPaneOpen" $
    return ()


-- | Page footer HTML
pageFooterHTML :: Assets -> Html
pageFooterHTML (Assets iconIndex) = do
  H.div ! A.class_ "page-footer" $ do
    H.div ! A.class_ "left-pane-button"
          ! X.onClick "historyPaneOpen = !historyPaneOpen" $
      H.div ! A.class_ "left-pane-button-icon" $ 
        H.preEscapedText $ fromJust $ iconSVGWithName "menu" iconIndex

