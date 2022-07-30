--
-- HTML.Comp.ViewSwitcher
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.ViewSwitcher (
    html
  ) where


import qualified Data.List as L (intercalate)
import           Data.Maybe (fromJust)
import qualified Data.Text as T (toUpper)
import qualified Data.Text.IO as T (putStrLn)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.Alpine as X
import qualified Web.HTML.Comp.StateButton as StateButton (html)
import qualified Web.Types.State as State (object, verb)
import qualified Web.Types.View as View (new)
import           Web.Types.State as State (State (..))


-- | View Switcher HTML
html :: State -> Assets -> Html
html st assets = do
  let view = View.new st
  X.html view "comp-view-switcher" $ do
    H.div ! classes [cls "button"]
          ! X.onClick "chooserOpen = !chooserOpen" $ do
      H.div ! classes [cls "verb"] $
        H.div ! classes [cls "verb-text"] $ 
          H.toMarkup $ T.toUpper $ State.verb view.state
      H.div ! classes [cls "object"] $ 
        H.toMarkup $ T.toUpper $ State.object view.state
      H.div ! classes [cls "expand"] $ 
        H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" assets.iconIndex
    H.div ! classes [cls "chooser"]
          ! X.show_ "chooserOpen" $ 
      chooserHTML assets



chooserHTML :: Assets -> Html
chooserHTML assets = do
  mainHTML assets
  storyHTML


mainHTML :: Assets -> Html
mainHTML assets = do
  H.div ! classes [cls "chooser-main"] $ do
    H.div ! classes [cls "chooser-header"] $ "MAIN"
    H.div ! classes [cls "chooser-main-states"] $ do
      choiceHTML State.MainHome assets
      choiceHTML State.MainCreateStory assets
      choiceHTML State.MainFindStory assets


storyHTML :: Html
storyHTML = return ()


choiceHTML :: State -> Assets -> Html
choiceHTML state assets = do
  H.div ! classes [cls "chooser-choice"] $ do
    StateButton.html state assets



-- | HTML helper combinators 
classes l = A.class_ $ H.toValue $ L.intercalate " " l
cls s = "comp-view-switcher-" <> s 
