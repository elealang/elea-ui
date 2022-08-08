--
-- HTML.Comp.StoryLink
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.StoryLink (
    html
  , Size (..)
  ) where


import qualified Data.Char as C (toLower)
import           Data.Maybe (fromJust)
import           Data.Text (Text)

import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)


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
    H.div ! classes [cls "story-name"] $ H.toMarkup storyName



classes :: [String] -> H.Attribute
classes l = A.class_ $ H.toValue $ unwords l

cls :: String -> String
cls s = "comp-story-link-" <> s 
