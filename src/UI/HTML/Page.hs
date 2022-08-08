--
-- HTML: Page
--

module UI.HTML.Page (
    html
  ) where


import           Control.Monad (when)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (toUpper)
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Alpine as X
import qualified UI.HTML.Document as Document (html)
import           UI.Types.Elea as Elea
import           UI.Types.Page (Page, PageHeader (..))


html :: Text -> EleaState -> Page -> Html
html script st page = do
  Document.html script $
    X.html page "page" $ do
      headerHTML page.header
      H.div ! A.class_ "page-main" $ do
        sidebarHTML page.sidebarHTML
        H.div ! A.class_ "page-content" $ page.contentHTML
        modalPaneHTML st
      footerHTML page
   

-- | Page header HTML
headerHTML :: PageHeader -> Html
headerHTML header = do
  H.div ! A.class_ "page-header" $ do
    H.div ! A.class_ "page-header-left" $
      header.leftHTML
    H.div ! A.class_ "page-header-center" $
      header.centerHTML
    H.div ! A.class_ "page-header-right" $
      header.rightHTML
    

-- | Page header HTML
sidebarHTML :: Html -> Html
sidebarHTML contentHTML = do
  H.div ! A.class_ "page-sidebar" 
        ! H.customAttribute "x-show" "storyPaneOpen" $
    contentHTML


-- | Page header HTML
modalPaneHTML :: EleaState -> Html
modalPaneHTML st = do
  let js = "modal_pane_open = false; "
        <> "previous_state_object = current_state_verb; "
        <> "previous_state_verb = current_state_object; "
        <> "current_state_verb = '" <> T.toUpper (Elea.stateVerb st) <> "'; " 
        <> "current_state_object = '" <> T.toUpper (Elea.stateObject st) <> "'; "
  H.div ! A.class_ "page-modal-pane" 
        ! H.customAttribute "x-show" "modal_pane_open"
        ! X.onClickOutside js $ do
    return ()


-- | Page footer HTML
footerHTML :: Page -> Html
footerHTML page = do
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

