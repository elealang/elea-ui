--
-- HTML: Page
--

module Web.HTML.Page (
    html
  ) where


import           Control.Monad (forM_, when)
import           Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import           Text.Blaze (customAttribute, preEscapedText)
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.Alpine as X
import qualified Web.HTML.Document as Document (html)
import qualified Web.HTML.Comp.StoryHistory as StoryHistory (html)
import           Web.Types.Page (Page)
import qualified Web.Types.Page as Page
import qualified Web.Types.View as View


html :: Page -> Html
html page = do
  Document.html $
    X.html page "page" $ do
      pageHeaderHTML page.assets page.headerCenterHTML
      H.div ! A.class_ "page-main" $ do
        pageSidebarHTML page.assets
        H.div ! A.class_ "page-content" $ page.contentHTML
      pageFooterHTML page
   

-- | Page header HTML
pageHeaderHTML :: Assets -> Html -> Html
pageHeaderHTML assets centerHTML = do 
  H.div ! A.class_ "page-header" $ do
    H.div ! A.class_ "page-title" $ do
      H.div ! A.class_ "page-title-name" $ "ELEA"
      H.div ! A.class_ "page-title-description" $ "ENGINE FOR ETHICAL CHANGE"
    centerHTML
    H.div ! A.class_ "page-account" $ return ()


-- | Page header HTML
pageSidebarHTML :: Assets -> Html
pageSidebarHTML assets = do
  H.div ! A.class_ "page-sidebar" 
        ! customAttribute "x-show" "storyPaneOpen" $ do
    StoryHistory.html Nothing assets


-- | Page footer HTML
pageFooterHTML :: Page -> Html
pageFooterHTML page = do
  H.div ! A.class_ "page-footer" $ do
    when page.showStoryButton $
      H.div ! A.class_ "left-pane-button"
            ! X.onClick "storyPaneOpen = !storyPaneOpen" $
        iconHTML
  where
    iconHTML = do
      H.div ! A.class_ "left-pane-button-icon" $ 
        H.preEscapedText $ fromJust $ 
          iconSVGWithName "story" page.assets.iconIndex

