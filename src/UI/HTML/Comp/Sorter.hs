--
-- Story Browser HTML Component
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.Sorter (
    html
  ) where


import           Control.Monad (when)
import           Data.Maybe (fromJust)
import qualified Data.Text as T (pack, unpack)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import qualified UI.HTML.Comp.StateButton as StateButton (html, ButtonType (..))
import qualified UI.Types.Elea as Elea

import           Elea.Base (Computer, Story)
import           Elea.Index (ComputerIndex)
import           Elea.Set (SetKind)
import qualified Elea.Set as Set (SetKind (..))
import qualified Elea.Server as Server (Kind (..))


-- | View HTML
html :: Assets -> Html
html assets = do
  H.div ! A.class_ "comp-sorter" $ do
    H.div ! classes [cls "content"] $ do
      H.div ! classes [cls "header", "comp-header"] $ do
        StateButton.html StateButton.Sidebar Elea.SortStories assets


storySetHTML :: Html
storySetHTML = do
  H.div ! A.class_ "comp-sorter-order-by set" $ do
    H.div ! A.class_ "set-story-content set-content" $ do
      H.div ! A.class_ "set-story-kind set-kind" $ "name is like"
      -- storySetNameHTML


-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "comp-sorter-" <> s 
