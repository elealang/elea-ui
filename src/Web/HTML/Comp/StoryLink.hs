--
-- HTML.Comp.StoryLink
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.StoryLink (
    html
  , Size (..)
  ) where


import Control.Monad (forM_)
import qualified Data.Char as C (toLower)
import qualified Data.List as L (intercalate)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze (preEscapedText, toMarkup, toValue)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)


-- | Story link size variations
data Size = 
    Small
  | Large
  deriving (Show)


-- | HTML
html :: Text -> Size -> Assets -> Html
html storyName size assets = do
  let sizeClass = "is-" <> (C.toLower <$> show size)
  H.div ! classes ["comp-story-link", sizeClass] $ do
    H.div ! classes [cls "icon"] $ do
      H.preEscapedText $ fromJust $ iconSVGWithName "story" assets.iconIndex
    H.div ! classes [cls "story-name"] $ toMarkup storyName


classes l = A.class_ $ toValue $ L.intercalate " " l
cls s = "comp-story-link-" <> s 
