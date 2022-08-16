--
-- HTML.Comp.StoryLink
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.Link (
    html
  ) where


import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)

import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)


html :: Text -> Text -> Assets -> Html
html storyName icon assets = do
  H.div ! classes ["comp-story-link"] $ do
    H.div ! classes [cls "icon", T.unpack icon] $ do
      H.preEscapedText $ fromJust $ iconSVGWithName icon assets.iconIndex
    H.div ! classes [cls "story-name"] $ H.toMarkup storyName


--html2 :: Text -> Text -> Assets -> Html
--html2 storyName icon assets = do


classes :: [String] -> H.Attribute
classes l = A.class_ $ H.toValue $ unwords l

cls :: String -> String
cls s = "comp-story-link-" <> s 
