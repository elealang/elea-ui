--
-- HTML.Comp.ViewSwitcher
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.ViewSwitcher (
    html
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Data.Text as T (toUpper)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze (toMarkup)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze (preEscapedText)

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)
import qualified Web.HTML.Alpine as X
import Web.Types.View (
    View
  , verb, object
  )


-- | View Switcher HTML
html :: View -> Assets -> Html
html view (Assets iconIndex) = do
  X.html view "comp-view-switcher" $ do
    H.div ! A.class_ "comp-view-switcher-button"
          ! X.onClick "chooserOpen = !chooserOpen" $ do
      H.div ! A.class_ "comp-view-switcher-verb" $
        H.div ! A.class_ "comp-view-switcher-verb-text" $ 
          toMarkup $ T.toUpper $ verb view.typ
      H.div ! A.class_ "comp-view-switcher-object" $ 
        toMarkup $ T.toUpper $ object view.typ
      H.div ! A.class_ "comp-view-switcher-expand" $ 
        H.preEscapedText $ fromJust $ iconSVGWithName "expand-arrow" iconIndex
    H.div ! A.class_ "comp-view-switcher-chooser" 
          ! X.show_ "chooserOpen" $ 
      return ()

