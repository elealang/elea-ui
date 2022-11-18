--
-- HTML.Comp.StoryLink
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.Link (
    programHTML
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


-- | Program HTML
programHTML :: Text -> Text -> Text -> Assets -> Html
programHTML programId programName icon assets = do
  H.a ! classes [cls "program"] 
      ! A.href (H.toValue $ "/elea/program/view/" <> "?effect=" <> programId) $ do
    H.div ! classes [cls "program-content", T.unpack icon] $ do
      H.div ! classes [cls "program-icon", T.unpack icon] $
        H.preEscapedText $ fromJust $ iconSVGWithName icon assets.iconIndex
      H.div ! classes [cls "program-name"] $ H.toMarkup programName


classes :: [String] -> H.Attribute
classes l = A.class_ $ H.toValue $ unwords l

cls :: String -> String
cls s = "comp-link-" <> s 
