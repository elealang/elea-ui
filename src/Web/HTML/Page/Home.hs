--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Page.Home (
    html
  ) where


import           Control.Monad (forM_)
import qualified Data.List as L (intercalate)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze (preEscapedText)

import           Data.Assets (Assets (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.Page as Page (html)
import qualified Web.HTML.Comp.StateButton as StateButton
import qualified Web.HTML.Comp.StoryLink as StoryLink
import           Web.Types.Page (Page (..))
import qualified Web.Types.State as State (State (..))


html :: Assets -> Html
html assets = Page.html page
  where
    headerHTML  = H.div ! A.class_ "header-home" $ return ()
    page        = Page {
        assets           = assets
      , storyPaneOpen    = False
      , showStoryButton  = False
      , headerCenterHTML = headerHTML
      , contentHTML      = pageHTML assets
    }


pageHTML :: Assets -> Html
pageHTML assets = do
  H.div ! A.class_ "page-home" $ do
    H.div ! A.class_ "page-home-main" $ do
      H.div ! A.class_ "page-home-main-left" $ do
        H.div ! A.class_ "page-home-main-left-text" $
          "Tell Your Story."
        H.div ! A.class_ "page-home-main-left-examples" $ do
          let linkSize = StoryLink.Large
          StoryLink.html "Building an App" linkSize assets
          StoryLink.html "Programing Your Mind" linkSize assets
          StoryLink.html "Playing a Game with Friends" linkSize assets
          StoryLink.html "Reading a Book" linkSize assets
          StoryLink.html "Analyzing the Economy " linkSize assets
          StoryLink.html "Finding a Hotel" linkSize assets
          StoryLink.html "Learning About Mathematics" linkSize assets
          StoryLink.html "Sharing Images with Friends" linkSize assets
          StoryLink.html "Taking out a Loan" linkSize assets
          StoryLink.html "Buying Stuff Online" linkSize assets
        H.div ! A.class_ "page-home-main-left-actions" $ do
          H.div ! A.class_ "page-home-main-left-actions-buttons" $ do
            StateButton.html State.MainCreateStory assets
            StateButton.html State.MainFindStory assets
      H.div ! A.class_ "page-home-main-right" $ do
        H.div ! A.class_ "page-home-browse" $ do
          H.div ! A.class_ "page-home-browse-abstractions" $ return ()
          H.div ! A.class_ "page-home-browse-computers" $ return ()
    H.div ! A.class_ "page-home-help" $ return ()
