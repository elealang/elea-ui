--
-- HTML: Page
--
-- The immutable structure of the HTML content paramterized by the program.
--

module UI.HTML.Program.Page (
    html
  ) where


import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Alpine as X
import qualified UI.HTML.Document as Document (html)
import qualified UI.HTML.Comp.ProgramSwitcher as ProgramSwitcher (html)
import qualified UI.HTML.Comp.ProgramStory as ProgramStory (html)
import           UI.Types.Component
import           UI.Types.Page (Page (..), PageComponentData)


import qualified Elea


-- | Generic page HTML
html :: Text -> Page -> Html -> Html
html script page stateHtml = do
  let pageCompData = toComponentData page :: PageComponentData
  Document.html script $
    X.html pageCompData "page" $ do
      headerHTML page
      contentHTML page stateHtml
      footerHTML page
      agentModalHTML 

-- | Page header
headerHTML :: Page -> Html
headerHTML page = do
  H.div ! A.class_ "page-header" $ do
    leftHTML
    centerHTML
    rightHTML
  where
    leftHTML = 
      H.div ! A.class_ "page-header-left" $ do
        H.div ! A.class_ "page-sidebar-button" $ 
          H.preEscapedText $ fromJust $ 
            iconSVGWithName "menu" page.assets.iconIndex
        ProgramSwitcher.html page
    centerHTML = 
      H.div ! A.class_ "page-header-center" $ 
        return ()
    rightHTML = 
      H.div ! A.class_ "page-header-right" $ do
        H.div ! A.class_ "page-header-agent" $
          "Person"
        --H.div ! A.class_ "preview-pane-button" $ do
          --H.div ! A.class_ "preview-pane-button-icon-close"
                -- ! X.onClick "previewPaneActive = !previewPaneActive"
                -- ! X.show_ "previewPaneActive" $
            --H.preEscapedText $ fromJust $ 
              --iconSVGWithName "pane-close" page.assets.iconIndex
          --H.div ! A.class_ "preview-pane-button-icon-open"
                -- ! X.onClick "previewPaneActive = !previewPaneActive"
                -- ! X.show_ "!previewPaneActive" $
            --H.preEscapedText $ fromJust $ 
              --iconSVGWithName "pane-open" page.assets.iconIndex

-- | Page content
contentHTML :: Page -> Html -> Html
contentHTML page stateHtml = do
  H.div ! A.class_ "page-content" $ do
    H.div ! A.class_ "page-content-layer1" $ do
      progContextHTML page
      H.div ! A.class_ "page-state"
            ! X.depClass "agentModalActive ? 'is-dimmed' : ''" $ do
        stateHtml
    H.div ! A.class_ "page-content-layer2" 
          ! H.customAttribute "x-show" "contentModalActive" $ 
      return ()

-- | Page footer HTML
footerHTML :: Page -> Html
footerHTML page = do
  H.div ! A.class_ "page-footer" $ do
    H.div ! A.class_ "left-pane-button" $ 
      iconHTML
  where
    iconHTML = do
      H.div ! A.class_ "left-pane-button-icon" $ do
        H.div ! A.class_ "left-pane-button-icon-close" 
              ! X.onClick "agentModalActive = !agentModalActive" $ 
              -- ! X.show_ "contextPaneActive" $
          H.preEscapedText $ fromJust $ 
            iconSVGWithName "open-window" page.assets.iconIndex
        --H.div ! A.class_ "left-pane-button-icon-open"
              -- ! X.onClick "contextPaneActive = !contextPaneActive"
              -- ! X.show_ "!contextPaneActive" $
          --H.preEscapedText $ fromJust $ 
            --iconSVGWithName "pane-close" page.assets.iconIndex

-- | Context
--------------------------------------------------------------------------------

-- | Context (Sidebar) HTML
progContextHTML :: Page -> Html
progContextHTML page = do
  H.div ! A.class_ "page-context"
        ! H.customAttribute "x-show" "contextPaneActive" $ 
    ProgramStory.html page




-- | Agent Modal HTML
agentModalHTML :: Html
agentModalHTML = do
  H.div ! A.class_ "agent-modal" 
         ! H.customAttribute "x-show" "agentModalActive"
         ! X.onClickOutside "agentModalActive = false;" $ do
    return ()



