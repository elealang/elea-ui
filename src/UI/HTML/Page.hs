--
-- HTML: Page
--
-- The structure of the HTML content paramterized by the program.
--

module UI.HTML.Page (
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
import qualified UI.HTML.Program.Develop as ProgDevelop
import qualified UI.HTML.Program.Elea as ProgElea
import           UI.Types.Component
import           UI.Types.Page (Page (..), PageComponentData)


import qualified Elea.Atom.Phenomena as EleaAtom (
    AbstractionId (..)
  )
import qualified Elea.Atom.Program as EleaAtom (
    ProgramAlias (..)
  )
import qualified Elea.Object.Phenomena as EleaObject (
    State (..)
  , StateName (..)
  )
import qualified Elea.Object.Program as EleaObject (
    programState
  )


-- | Generic page HTML
html :: Text -> Page -> Html
html script page = do
  let pageCompData = toComponentData page :: PageComponentData
  Document.html script $
    X.html pageCompData "page" $ do
      headerHTML page
      H.div ! A.class_ "page-content" $ do
        progContextHTML page
        progStateHTML page
      footerHTML page

-- | Page header
headerHTML :: Page -> Html
headerHTML page = do
  H.div ! A.class_ "page-header" $ do
    progContextHeaderHTML page
    --toolbarHeaderHTML page

-- | Page footer HTML
footerHTML :: Page -> Html
footerHTML page = do
  H.div ! A.class_ "page-footer" $ do
    H.div ! A.class_ "left-pane-button"
          ! X.onClick "storyPaneOpen = !storyPaneOpen" $
      iconHTML
  where
    iconHTML = do
      H.div ! A.class_ "left-pane-button-icon" $ 
        H.preEscapedText $ fromJust $ 
          iconSVGWithName "story" page.assets.iconIndex

-- | Context
--------------------------------------------------------------------------------

-- | Context (Sidebar) HTML
progContextHTML :: Page -> Html
progContextHTML page = do
  H.div ! A.class_ "page-context"
        ! H.customAttribute "x-show" "contextPaneOpen" $ 
    return ()

progContextHeaderHTML :: Page -> Html
progContextHeaderHTML page =
  H.div ! A.class_ "page-context-header" $ do
    H.div ! A.class_ "page-context-header-program page-context-header-button" $
      H.toMarkup nameText
    H.div ! A.class_ "page-context-header-divider" $ 
      H.preEscapedText $ fromJust $ 
        iconSVGWithName "chevron-right" page.assets.iconIndex
    H.div ! A.class_ "page-context-header-state page-context-header-button" $
      H.toMarkup stateText

  where
    nameText :: Text
    nameText = page.program.id.name.value
    stateText :: Text
    stateText = 
      let  mState = EleaObject.programState 
                      page.program 
                      page.programState.abstractionId
                      page.programState.state
      in case mState of
        Just (EleaObject.State _ (EleaObject.StateName t) _) -> t
        Nothing                                              -> "Unknown"
      

-- | Main
--------------------------------------------------------------------------------

-- | Main page content HTML
progStateHTML :: Page -> Html
progStateHTML page = do
  H.div ! A.class_ "page-state" $ do
    let (EleaAtom.ProgramAlias programAlias) = page.program.id.alias
    case programAlias of
      "develop" -> ProgDevelop.html page
      "elea"    -> ProgElea.html page
      _         -> return ()


-- | Page header HTML
--modalPaneHTML :: EleaState -> Html
--modalPaneHTML st = do
  --let js = "modal_pane_open = false; "
        -- <> "previous_state_object = current_state_verb; "
        -- <> "previous_state_verb = current_state_object; "
        -- <> "current_state_verb = '" <> T.toUpper (Elea.stateVerb st) <> "'; " 
        -- <> "current_state_object = '" <> T.toUpper (Elea.stateObject st) <> "'; "
  --H.div ! A.class_ "page-modal-pane" 
        -- ! H.customAttribute "x-show" "modal_pane_open"
        -- ! X.onClickOutside js $ do
    --return ()



