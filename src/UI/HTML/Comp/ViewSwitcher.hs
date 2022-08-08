--
-- HTML.Comp.ViewSwitcher
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.ViewSwitcher (
    html
  ) where


import           Data.Maybe (fromJust)
import qualified Data.Text as T (toUpper, unpack)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Alpine as X
import qualified UI.HTML.Comp.StateButton as StateButton
import           UI.Types.Elea as Elea (
    EleaState (..)
  , stateObject, stateVerb, stateRoute
  , newView
  )


-- | View Switcher HTML
html :: Assets -> Html
html assets = do
  let view = newView
  X.html view "comp-view-switcher" $ do
    H.div ! classes [cls "button"]
          ! X.onClick "chooserOpen = !chooserOpen" $ do
      H.div ! classes [cls "verb"] $
        H.div ! classes [cls "verb-text"] 
              ! X.text "current_state_verb" $ return () 
      H.div ! classes [cls "object"]
            ! X.text "current_state_object" $ return () 
      H.div ! classes [cls "icon"] $ do
        H.div ! classes [cls "icon-expand"] 
              ! X.show_ "!modal_pane_open" $ 
          H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" assets.iconIndex
        H.div ! classes [cls "icon-close"]
              ! X.show_ "modal_pane_open" $ 
          H.preEscapedText $ fromJust $ iconSVGWithName "close" assets.iconIndex
    H.div ! classes [cls "chooser"]
          ! X.show_ "chooserOpen" $ 
      chooserHTML assets


chooserHTML :: Assets -> Html
chooserHTML assets = do
  chooserDevelopHTML assets


chooserDevelopHTML :: Assets -> Html
chooserDevelopHTML assets = do
  chooserDevelopHeaderHTML
  chooserDevelopContentHTML assets

chooserDevelopHeaderHTML :: Html
chooserDevelopHeaderHTML = do
  H.div ! classes [cls "chooser-header"] $ do
    H.div ! classes [cls "chooser-header-category"] $ "DEVELOP"
    H.div ! classes [cls "chooser-header-subcategory"] $ "GENERAL"
    H.div ! classes [cls "chooser-header-subcategory"] $ "LANGUAGE"
    H.div ! classes [cls "chooser-header-subcategory"] $ "INDEXES"


chooserDevelopContentHTML :: Assets -> Html
chooserDevelopContentHTML assets = do
  H.div ! classes [cls "chooser-main"] $ do
    H.div ! classes [cls "chooser-category"] $ return ()
    H.div ! classes [cls "chooser-subcategory"] $ do
      choiceHTML Elea.GeneralHome assets
    H.div ! classes [cls "chooser-subcategory"] $ do
      choiceHTML Elea.EleaCreateStory assets
      choiceHTML Elea.EleaCreateProgram assets
      choiceHTML Elea.EleaCreateAbstraction assets
      choiceHTML Elea.EleaCreateState assets
      choiceHTML Elea.EleaCreateArrow assets
    H.div ! classes [cls "chooser-subcategory"] $ do
      choiceHTML Elea.IndexFindStory assets
      choiceHTML Elea.IndexFindProgram assets
      choiceHTML Elea.IndexFindAbstraction assets


--chooserAdminHTML :: Assets -> Html
--chooserAdminHTML assets = do
  --chooserAdm
  --chooserDevelopContentHTML assets


storyHTML :: Html
storyHTML = return ()


choiceHTML :: EleaState -> Assets -> Html
choiceHTML state assets = do
  let stateClass = T.unpack $ Elea.stateVerb state <> "-" <> Elea.stateObject state
  H.div ! classes [cls "chooser-choice", cls stateClass] $ do
    StateButton.html StateButton.Inline state assets



-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes l = A.class_ $ H.toValue $ unwords l

cls :: String -> String
cls s = "comp-view-switcher-" <> s 
