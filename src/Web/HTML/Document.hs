--
-- HTML: Dcoument
--

module Web.HTML.Document (
    html
  ) where


import           Text.Blaze.Html (Html)
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A


-- | Document HTML
html :: Html -> Html
html pageHtml = H.docTypeHtml $ do
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
    -- CSS/Page: Home
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/page-home.css"
    -- CSS/Page: Build
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/page-build.css"
    -- CSS/Component: ArrowEditor
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-arrow-editor.css"
    -- CSS/Component: Form
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-form.css"
    -- CSS/Component: ComputerChooser
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-computer-chooser.css"
    -- CSS/Component: ProgramEditor
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-program-editor.css"
    -- CSS/Component: StateButton
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-state-button.css"
    -- CSS/Component: StoryEditor
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-story-editor.css"
    -- CSS/Component: StoryHistory
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-story-history.css"
    -- CSS/Component: StoryLink
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-story-link.css"
    -- CSS/Component: ViewSwitcher
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/comp-view-switcher.css"
    -- Alpine
    H.preEscapedString "<script defer src='https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js'></script>"
    -- HTMX
    H.preEscapedString "<script defer src='/static/js/htmx.min.js'></script>"
    -- Fix Firefox refresh issue
    H.preEscapedString "<script>let FF_FOUC_FIX;</script>"
  H.body pageHtml

