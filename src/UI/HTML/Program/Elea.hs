--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Program.Elea (
    html
  ) where


import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Types.Page (Page)
import qualified UI.HTML.Program.Elea.View as View (html)

import           Elea.Atom.Phenomena (State (..))


-- | assuming just one abstraction
html :: Page -> Html
html page = case page.programState.state of
  State "view" -> View.html page.assets
  _            -> return ()


---- | View HTML
--html :: PageElea -> Assets -> Html
--html page _assets =  Page.html "script goes here" page.state Page {
      --assets              = _assets
    --, storyPaneOpen       = True
    --, modalPaneOpen       = False
    --, showStoryButton     = True
    --, currentStateVerb    = T.toUpper $ Elea.stateVerb page.state
    --, currentStateObject  = T.toUpper $ Elea.stateObject page.state
    --, previousStateVerb   = Nothing
    --, previousStateObject = Nothing
    --, header          = PageHeader {
          ---- leftHTML   = ViewSwitcher.html _assets
          --leftHTML   = return ()
        --, centerHTML = return ()
        --, rightHTML  = headerEleaHTML
        ----, rightHTML   = headerStoryHTML _assets
      --}
    --, contentHTML     = pageHTML page _assets
    --, sidebarHTML     = return ()
  --}
  --where
    ---- arrowJSON = T.pack $ LBS.unpack $ encode $ Elea.arrow page.state
    ---- script = "Elea.server.getComputer('computer.elea.software/story').appendToStory('my-dev-story', " <> arrowJSON <> " );"


---- | Page HTMl
--pageHTML :: PageElea -> Assets -> Html
--pageHTML page _assets = do
  --H.div ! A.class_ "page-elea" $ do
    --defineHTML page.state _assets
    ---- computeHTML page.state page.config.servers _assets
    --resultsHTML page.state _assets


--headerStoryHTML :: Assets -> Html
--headerStoryHTML _assets = do
  --H.div ! A.class_ "page-elea-header-story" $ do
    --H.div ! A.class_ "page-elea-header-story-menu-button" $
      --H.preEscapedText $ fromJust $ iconSVGWithName "menu-vertical" _assets.iconIndex
    --H.div ! A.class_ "page-elea-header-story-info" $ do
      --H.div ! A.class_ "page-elea-header-story-name" $ "Browsing History"


--headerEleaHTML :: Html
--headerEleaHTML = do
  --H.a ! A.class_ "page-elea-header-title" 
      -- ! A.href "/" $ do
    --H.div ! A.class_ "page-elea-header-program-name" $ "Elea App"


---- | Define HTML
--defineHTML :: Elea.EleaState -> Assets -> Html
--defineHTML st a = do
  --H.div ! classes [cls "define"] $ do
    --case st of
      --Elea.GeneralHome     -> do
        --homeHeaderHTML
        --Home.html a
      --Elea.EleaCreateStory -> do
        --headerHTML
        --defineSectionHTML "NEW" "STORY" (StoryEditor.html a) a
      --Elea.IndexFindStory  -> do
        --headerHTML
        ----defineSectionHTML "SET OF" "SERVERS" (SetBuilder.html Set.Servers a) a
        ----defineSectionHTML "SET OF" "COMPUTERS" (SetBuilder.html Set.Computers a) a
        ----defineSectionHTML "SET OF" "STORIES" (SetBuilder.html Set.Stories a) a
      --_                    -> return ()
  --where
    --homeHeaderHTML =
      --H.div ! classes [cls "define-home-header", cls "section-header"] $
        --H.div ! classes [cls "anyone"] $ "anyone can program anything"
    --headerHTML = 
      --H.div ! classes [cls "define-header", cls "section-header"] $ do
        --H.div ! classes [cls "section-header-label"] $ "DEFINE"
        --H.div ! classes [cls "section-header-icon"] $ 
          --H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" a.iconIndex


---- | Define section HTML
--defineSectionHTML :: Text -> Text -> Html -> Assets -> Html
--defineSectionHTML _type _name compHTML a = do
  --H.div ! classes [cls "define-section", cls "section"] $ do
    --editorSidebarHTML _type _name a
    --H.div ! classes [cls "section-workspace"] $ 
      --compHTML 


--computeHTML :: Elea.EleaState -> Assets -> Html
--computeHTML st a = do
  --H.div ! classes [cls "compute"] $ case st of
    --Elea.GeneralHome     -> return ()
    --Elea.EleaCreateStory -> do 
      --headerHTML
      ---- let compHTML = ComputerChooser.html (ComputerChooser.new servers DefStory) a
      ---- computeSectionHTML "PROOF OF" "STORY" compHTML a
    --Elea.IndexFindStory  -> do
      --headerHTML
      --computeSectionHTML "ORDERING OF" "STORIES" (Sorter.html a) a
    --_                    -> return ()
  --where
    --headerHTML =
      --H.div ! classes [cls "compute-header", cls "section-header"] $ do
        --H.div ! classes [cls "section-header-label"] $ "COMPUTE"
        --H.div ! classes [cls "section-header-icon"] $ 
          --H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" a.iconIndex

---- | Compute HTML
--computeSectionHTML :: Text -> Text -> Html -> Assets -> Html
--computeSectionHTML _type _name compHTML a = do
  --H.div ! classes [cls "compute-section", cls "section"] $ do
    --editorSidebarHTML _type _name a
    --H.div ! classes [cls "section-workspace"] $ 
      --compHTML 


---- | Sidebar HTML
--editorSidebarHTML :: Text -> Text -> Assets -> Html
--editorSidebarHTML _type _name _assets = do
  --H.div ! A.class_ "page-elea-editor-sidebar" $ do
    --H.div ! A.class_ "page-elea-editor-sidebar-type" $ H.toMarkup _type
    --H.div ! A.class_ "page-elea-editor-sidebar-name" $ H.toMarkup _name


---- | Define HTML
--resultsHTML :: Elea.EleaState -> Assets -> Html
--resultsHTML st a = do
  --H.div ! classes [cls "results"] $ do
    --H.div ! classes [cls "results-header", cls "section-header"] $ do
      --H.div ! classes [cls "section-header-label"] $ "RESULTS"
      --H.div ! classes [cls "section-header-icon"] $ 
        --H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" a.iconIndex
    --case st of
      ---- Elea.IndexFindStory  -> do
        ---- resultsSectionHTML "SET OF" "SERVERS" (List.html Set.Stories a) a
      --_                    -> return ()


--resultsSectionHTML :: Text -> Text -> Html -> Assets -> Html
--resultsSectionHTML _type _name compHTML a = do
  --H.div ! classes [cls "results-section", cls "section"] $ do
    --editorSidebarHTML _type _name a
    --H.div ! classes [cls "section-workspace"] $ 
      --compHTML 


-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "page-elea-" <> s 
