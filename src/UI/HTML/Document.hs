--
-- HTML: Dcoument
--

module UI.HTML.Document (
    html
  ) where


import           Data.Text (Text)
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A


-- | Document HTML
html :: Text -> Html -> Html
html script pageHtml = H.docTypeHtml $ do
  H.head $ do
    H.title "Elea"
    -- CSS: Vars
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/vars.css"
    -- CSS: Page
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/page.css"
    -- CSS/Page: Build
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/page-elea.css"
    -- CSS/Component: ArrowEditor
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-arrow-editor.css"
    -- CSS/Component: ComputerChooser
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-computer-chooser.css"
    -- CSS/Component: Home
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-home.css"
    -- CSS/Component: Form
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-form.css"
    -- CSS/Component: Link
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-link.css"
    -- CSS/Component: List
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-list.css"
    -- CSS/Component: ProgramEditor
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-program-editor.css"
    -- CSS/Component: SetBuilder
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-set-builder.css"
    -- CSS/Component: Sorter
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-sorter.css"
    -- CSS/Component: StateButton
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-state-button.css"
    -- CSS/Component: StoryBrowser
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-story-browser.css"
    -- CSS/Component: StoryEditor
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-story-editor.css"
    -- CSS/Component: StoryHistory
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-story-history.css"
    -- CSS/Component: ViewSwitcher
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-view-switcher.css"
    -- Alpine
    H.preEscapedString "<script defer src='/static/js/alpine.min.js'></script>"
    -- HTMX
    H.preEscapedString "<script defer src='/static/js/htmx.min.js'></script>"
    -- Muuri
    H.preEscapedString "<script src='static/js/muuri.min.js'></script>"
    -- Elea browser server 
    H.preEscapedString "<script src='/static/js/computer-story.js'></script>"
    -- Fix Firefox refresh issue
    H.preEscapedString "<script>let FF_FOUC_FIX;</script>"
    H.preEscapedText $ "<script>" <> script <> "</script>"
  H.body pageHtml

