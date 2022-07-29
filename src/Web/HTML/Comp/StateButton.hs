--
-- HTML.Comp.StateButton
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.StateButton (
    html
  , Size (..)
  ) where


import Control.Monad (forM_)
import qualified Data.Char as C (toLower)
import qualified Data.Map as M (lookup, fromList)
import qualified Data.List as L (intercalate)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (toLower)

import Text.Blaze (preEscapedText, toMarkup, toValue)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)
import qualified Web.HTML.HTMX as HX


-- | Story link size variations
data Size = 
    Small
  | Large
  deriving (Show)


-- | HTML
html :: Text -> Text -> Assets -> Html
html stateVerb stateObject assets = do
  let route = T.toLower stateVerb <> "-" <> T.toLower stateObject
  H.a ! classes ["comp-state-button"] 
      ! A.href "/create-story" $ do
    H.div ! classes [cls "content"] $ do
      H.div ! classes [cls "icon"] $ iconByVerb stateVerb assets
      H.div ! classes [cls "verb"] $ toMarkup stateVerb
      H.div ! classes [cls "object"] $ toMarkup stateObject


iconByVerb :: Text -> Assets -> Html
iconByVerb verb assets = do
  let verb_ = T.toLower verb
      iconName = fromJust $ M.lookup verb_ iconNameByVerb
  H.preEscapedText $ fromJust $ iconSVGWithName iconName assets.iconIndex
  where
    iconNameByVerb = M.fromList [
        ("create", "plus")
      , ("find", "search")
      ]


classes l = A.class_ $ toValue $ L.intercalate " " l
cls s = "comp-state-button-" <> s 
