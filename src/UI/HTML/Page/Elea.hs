--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Page.Elea (
    html
  ) where


import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (pack, toUpper)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Alpine as X
import qualified UI.HTML.Comp.ComputerChooser as ComputerChooser
import qualified UI.HTML.Comp.Home as Home
import qualified UI.HTML.Comp.List as List
import qualified UI.HTML.Comp.SetBuilder as SetBuilder
import qualified UI.HTML.Comp.Sorter as Sorter
import qualified UI.HTML.Comp.StateButton as StateButton
import qualified UI.HTML.Comp.StoryEditor as StoryEditor
import qualified UI.HTML.Comp.StoryHistory as StoryHistory (html)
import qualified UI.HTML.Comp.ViewSwitcher as ViewSwitcher
import qualified UI.HTML.Page as Page (html)
import qualified UI.Types.ComputerChooser as ComputerChooser
import qualified UI.Types.Elea (arrow)
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
html page _assets =  Page.html script page.state Page {
      assets              = _assets
    , storyPaneOpen       = True
    , modalPaneOpen       = False
    , showStoryButton     = True
    , currentStateVerb    = T.toUpper $ Elea.stateVerb page.state
    , currentStateObject  = T.toUpper $ Elea.stateObject page.state
    , previousStateVerb   = Nothing
    , previousStateObject = Nothing
    , header          = PageHeader {
          leftHTML  = ViewSwitcher.html _assets
        , centerHTML = headerEleaHTML
        , rightHTML   = headerStoryHTML _assets
      }
    , contentHTML     = pageHTML page _assets
    , sidebarHTML     = StoryHistory.html Nothing _assets
  }
  where
    arrowJSON = T.pack $ LBS.unpack $ encode $ Elea.arrow page.state
    script = "Elea.server.getComputer('computer.elea.software/story').appendToStory('my-dev-story', " <> arrowJSON <> " );"


-- | Page HTMl
pageHTML :: PageElea -> Assets -> Html
pageHTML page _assets = do
  H.div ! A.class_ "page-elea" $ do
    defineHTML page.state _assets
    computeHTML page.state page.config.servers _assets
    resultsHTML page.state _assets


headerStoryHTML :: Assets -> Html
headerStoryHTML _assets = do
  H.div ! A.class_ "page-elea-header-story" $ do
    H.div ! A.class_ "page-elea-header-story-menu-button" $
      H.preEscapedText $ fromJust $ iconSVGWithName "menu-vertical" _assets.iconIndex
    H.div ! A.class_ "page-elea-header-story-info" $ do
      H.div ! A.class_ "page-elea-header-story-name" $ "Browsing History"


headerEleaHTML :: Html
headerEleaHTML = do
  H.a ! A.class_ "page-elea-header-title" 
      ! A.href "/" $ do
    H.div ! A.class_ "page-elea-header-program-name" $ "Elea App"


-- | Define HTML
defineHTML :: Elea.EleaState -> Assets -> Html
defineHTML st a = do
  H.div ! classes [cls "define"] $ do
    case st of
      Elea.GeneralHome     -> do
        homeHeaderHTML
        Home.html a
      Elea.EleaCreateStory -> do
        headerHTML
        defineSectionHTML "NEW" "STORY" (StoryEditor.html a) a
      Elea.IndexFindStory  -> do
        headerHTML
        defineSectionHTML "SET OF" "SERVERS" (SetBuilder.html Set.Servers a) a
        defineSectionHTML "SET OF" "COMPUTERS" (SetBuilder.html Set.Computers a) a
        defineSectionHTML "SET OF" "STORIES" (SetBuilder.html Set.Stories a) a
      _                    -> return ()
  where
    homeHeaderHTML =
      H.div ! classes [cls "define-home-header", cls "section-header"] $
        H.div ! classes [cls "anyone"] $ "anyone can program anything"
    headerHTML = 
      H.div ! classes [cls "define-header", cls "section-header"] $ do
        H.div ! classes [cls "section-header-label"] $ "DEFINE"
        H.div ! classes [cls "section-header-icon"] $ 
          H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" a.iconIndex


-- | Define section HTML
defineSectionHTML :: Text -> Text -> Html -> Assets -> Html
defineSectionHTML _type _name compHTML a = do
  H.div ! classes [cls "define-section", cls "section"] $ do
    editorSidebarHTML _type _name a
    H.div ! classes [cls "section-workspace"] $ 
      compHTML 


computeHTML :: Elea.EleaState -> [Server] -> Assets -> Html
computeHTML st servers a = do
  H.div ! classes [cls "compute"] $ case st of
    Elea.GeneralHome     -> return ()
    Elea.EleaCreateStory -> do 
      headerHTML
      let compHTML = ComputerChooser.html (ComputerChooser.new servers DefStory) a
      computeSectionHTML "PROOF OF" "STORY" compHTML a
    Elea.IndexFindStory  -> do
      headerHTML
      computeSectionHTML "ORDERING OF" "STORIES" (Sorter.html a) a
    _                    -> return ()
  where
    headerHTML =
      H.div ! classes [cls "compute-header", cls "section-header"] $ do
        H.div ! classes [cls "section-header-label"] $ "COMPUTE"
        H.div ! classes [cls "section-header-icon"] $ 
          H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" a.iconIndex

-- | Compute HTML
computeSectionHTML :: Text -> Text -> Html -> Assets -> Html
computeSectionHTML _type _name compHTML a = do
  H.div ! classes [cls "compute-section", cls "section"] $ do
    editorSidebarHTML _type _name a
    H.div ! classes [cls "section-workspace"] $ 
      compHTML 


-- | Sidebar HTML
editorSidebarHTML :: Text -> Text -> Assets -> Html
editorSidebarHTML _type _name _assets = do
  H.div ! A.class_ "page-elea-editor-sidebar" $ do
    H.div ! A.class_ "page-elea-editor-sidebar-type" $ H.toMarkup _type
    H.div ! A.class_ "page-elea-editor-sidebar-name" $ H.toMarkup _name


-- | Define HTML
resultsHTML :: Elea.EleaState -> Assets -> Html
resultsHTML st a = do
  H.div ! classes [cls "results"] $ do
    H.div ! classes [cls "results-header", cls "section-header"] $ do
      H.div ! classes [cls "section-header-label"] $ "RESULTS"
      H.div ! classes [cls "section-header-icon"] $ 
        H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" a.iconIndex
    case st of
      Elea.IndexFindStory  -> do
        resultsSectionHTML "SET OF" "SERVERS" (List.html Set.Stories a) a
      _                    -> return ()


resultsSectionHTML :: Text -> Text -> Html -> Assets -> Html
resultsSectionHTML _type _name compHTML a = do
  H.div ! classes [cls "results-section", cls "section"] $ do
    editorSidebarHTML _type _name a
    H.div ! classes [cls "section-workspace"] $ 
      compHTML 


-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "page-elea-" <> s 
