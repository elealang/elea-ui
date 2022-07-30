--
-- HTML.Comp.StateButton
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.StateButton (
    html
  ) where


import           Control.Monad (forM_)
import qualified Data.Map as M (lookup, fromList)
import qualified Data.List as L (intercalate)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (toLower, toUpper)

import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.HTMX as HX
import           Web.Types.State (State)
import qualified Web.Types.State as State


-- | HTML
html :: State -> Assets -> Html
html st assets = do
  H.a ! classes ["comp-state-button"] 
      ! A.href (H.toValue $ State.route st) $ do
    H.div ! classes [cls "content"] $ do
      H.div ! classes [cls "icon"] $ iconByVerb (State.verb st) assets
      H.div ! classes [cls "verb"] $ H.toMarkup $ T.toUpper $ State.verb st
      H.div ! classes [cls "object"] $ H.toMarkup $ T.toUpper $ State.object st


iconByVerb :: Text -> Assets -> Html
iconByVerb verb assets = do
  let verb_ = T.toLower verb
      iconName = fromJust $ M.lookup verb_ iconNameByVerb
  H.preEscapedText $ fromJust $ iconSVGWithName iconName assets.iconIndex
  where
    iconNameByVerb = M.fromList [
        ("create", "plus")
      , ("find", "search")
      , ("go to", "arrow-right")
      ]


classes l = A.class_ $ H.toValue $ L.intercalate " " l
cls s = "comp-state-button-" <> s 
