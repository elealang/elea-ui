--
-- Story Browser HTML Component
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.StoryBrowser (
    html
  ) where


import           Data.Maybe (fromJust)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Comp.SetBuilder as SetBuilder (html)
import qualified UI.HTML.Comp.StateButton as StateButton (html, ButtonType (..))
-- import qualified UI.Types.Elea as Elea (EleaState (..))

--import           Elea.Base (Computer, Story)
--import           Elea.Index (ComputerIndex)
--import qualified Elea.Set as Set (SetKind (..))


-- | View HTML
html :: Assets -> Html
html assets = do
  H.div ! A.class_ "comp-story-browser" $ do
    H.div ! classes [cls "content"] $ do
      H.div ! classes [cls "header", "comp-header"] $ return ()
        -- StateButton.html StateButton.Inline Elea.IndexFindStory assets


--searchHTML :: Assets -> Html
--searchHTML assets = do
  --H.div ! classes [cls "search"] $ do
    --H.div ! classes [cls "search-server-set", cls "search-section"] $ do
      --H.div ! classes [cls "search-server-content-container"] $
        --SetBuilder.html Set.Servers assets
      --H.div ! classes [cls "search-server-button-container"] $
        --StateButton.html StateButton.Sidebar Elea.CollectServers assets
    --H.div ! classes [cls "search-story-set", cls "search-section"] $ do
      --H.div ! classes [cls "search-server-content-container"] $
        --SetBuilder.html Set.Stories assets
      --H.div ! classes [cls "search-server-button-container"] $
        --StateButton.html StateButton.Sidebar Elea.CollectStories assets
    --H.div ! classes [cls "search-computer-set", cls "search-section"] $ do
      --H.div ! classes [cls "search-server-content-container"] $
        --SetBuilder.html Set.Computers assets
      --H.div ! classes [cls "search-server-button-container"] $
        --StateButton.html StateButton.Sidebar Elea.CollectComputers assets
    ---- H.div ! classes [cls "search-compute", cls "search-section"] $ return ()
    

--resultsHTML :: Html
--resultsHTML = do
  --H.div ! classes [cls "results"] $ do
    --H.div ! classes [cls "results-header"] $ return ()
      ---- H.div ! classes [cls "results-header-label"] $ "STORIES" 
    --H.div ! classes [cls "results-list"] $ return ()
      ---- resultHTML


--resultHTML :: Story -> Computer -> ComputerIndex -> Html
--resultHTML story computer computerIndex = do
  --H.div ! classes [cls "results-list"] $ return ()
  

-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "comp-story-browser-" <> s 
