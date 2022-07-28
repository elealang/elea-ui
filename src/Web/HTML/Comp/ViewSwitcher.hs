--
-- HTML.Comp.ViewSwitcher
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.ViewSwitcher (
    html
  ) where


import           Data.Maybe (fromJust)
import qualified Data.Text as T (toUpper)
import qualified Data.Text.IO as T (putStrLn)

import           Text.Blaze (toMarkup)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.Alpine as X
import qualified Web.Types.State as St (object, verb)
import qualified Web.Types.View as View (new)
import           Web.Types.State (State)


-- | View Switcher HTML
html :: State -> Assets -> Html
html state (Assets iconIndex) = do
  let view = View.new state
  X.html view "comp-view-switcher" $ do
    H.div ! A.class_ "comp-view-switcher-button"
          ! X.onClick "chooserOpen = !chooserOpen" $ do
      H.div ! A.class_ "comp-view-switcher-verb" $
        H.div ! A.class_ "comp-view-switcher-verb-text" $ 
          toMarkup $ T.toUpper $ St.verb view.state
      H.div ! A.class_ "comp-view-switcher-object" $ 
        toMarkup $ T.toUpper $ St.object view.state
      H.div ! A.class_ "comp-view-switcher-expand" $ 
        H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" iconIndex
    H.div ! A.class_ "comp-view-switcher-chooser" 
          ! X.show_ "chooserOpen" $ 
      return ()

