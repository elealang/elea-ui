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
    -- CSS | program/develop/main/home.css
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/program/develop/main/home.css"
    -- CSS | program/elea/program/view-basic.css
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/program/elea/program/view-basic.css"
    -- CSS | theme/default/comp/form.css
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/theme/default/comp/form.css"
    -- CSS | theme/default/comp/link.css
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/theme/default/comp/link.css"
    -- CSS | theme/default/program-story.css
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/theme/default/program-story.css"
    -- CSS | theme/default/program-switcher.css
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/theme/default/program-switcher.css"
    -- CSS | theme/default/program.css
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/theme/default/program.css"
    -- CSS | theme/default/theme.css
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/theme/default/theme.css"
    -- JS | Alpine
    H.preEscapedString "<script defer src='/static/js/alpine.min.js'></script>"
    -- JS | HTMX
    H.preEscapedString "<script defer src='/static/js/htmx.min.js'></script>"
    -- JS | Muuri
    --H.preEscapedString "<script src='static/js/muuri.min.js'></script>"
    -- Elea browser server 
    H.preEscapedString "<script src='/static/js/computer-story.js'></script>"
    -- Fix Firefox refresh issue
    H.preEscapedString "<script>let FF_FOUC_FIX;</script>"
    H.preEscapedText $ "<script>" <> script <> "</script>"
  H.body pageHtml

