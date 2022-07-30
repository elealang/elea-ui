--
-- HTML.Comp.ComputerChooser
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.StoryHistory (
    html
  ) where


import           Data.Maybe (fromJust)
import qualified Data.List as L (intercalate)

import           Text.Blaze (toMarkup, toValue)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.Alpine as X
import qualified Web.HTML.Comp.StateButton as StateButton (html)
import qualified Web.Types.State as State (State (..))

import Elea.Base (Story (..))


-- | Story History HTML 
html :: Maybe Story -> Assets -> Html
html mStory assets = do
  H.div ! A.class_ "comp-story-history" $ do
    headerHTML mStory
    storyHTML mStory assets


headerHTML :: Maybe Story -> Html
headerHTML mStory = do
  H.div ! classes [cls "header"] $ do
    case mStory of
      Just story -> storyHTML story
      Nothing    -> noStoryHTML
  where
    storyHTML story = do
      H.div ! classes [cls "header-story"] $ 
        toMarkup story.name
    noStoryHTML = do
      H.div ! classes [cls "header-no-story"] $ do
        "No Active Story"


storyHTML :: Maybe Story -> Assets -> Html
storyHTML mStory assets = do
  H.div ! classes [cls "story"] $ do
    case mStory of
      Just story -> historyHTML story assets
      Nothing    -> findStoryHTML assets
  

findStoryHTML :: Assets -> Html
findStoryHTML assets = 
  H.div ! classes [cls "find-story"] $ do
    StateButton.html State.MainFindStory assets


historyHTML :: Story -> Assets -> Html
historyHTML _ _ = return ()

-- | HTML helper combinators 
classes l = A.class_ $ toValue $ L.intercalate " " l
cls s = "comp-story-history-" <> s 
