--
-- HTML.Comp.AbstractionManager
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.AbstractionManager (
    html
  ) where

import Control.Monad (forM_)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)


-- | Page HTML
html :: Assets -> Html
html assets = do
  H.div ! A.class_ "comp-abstraction-manager is-manager" $ do
    H.div ! A.class_ "comp-abstraction-manager-header" $ do
      H.div ! A.class_ "comp-abstraction-manager-title is-title" $ "ABSTRACTIONS"
      H.div ! A.class_ "comp-abstraction-manager-subtitle is-subtitle" $ "STRUCTURES"

