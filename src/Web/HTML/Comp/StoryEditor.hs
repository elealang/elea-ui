--
-- Story Editor HTML Component
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.StoryEditor (
    html
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (preEscapedText)

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)
import qualified Web.HTML.Comp.Form as Form
import Web.Types.Form (
    Form (..)
  , Field (..)
  , FieldType (..)
  )


-- | View HTML
html :: Assets -> Html
html assets = do
  H.div ! A.class_ "comp-story-editor" $ do
    H.div ! A.class_ "comp-story-editor-content" $ do
      H.div ! A.class_ "comp-story-editor-header comp-header" $ do
        H.div ! A.class_ "comp-story-editor-title comp-header-title" $ "STORY"
        H.div ! A.class_ "comp-story-editor-subtitle comp-header-subtitle" $ "CHANGE"
      H.div ! A.class_ "comp-story-editor-pane is-pane" $ do
        H.div ! A.class_ "comp-story-editor-form" $ do
          Form.html assets form
  

form :: Form
form = Form {
    fields = [
      Field {
          label = "name"
        , defaultValue = Nothing
        , fieldType = FieldTypeText
        , markupId = "name"
      },
      Field {
          label = "description"
        , defaultValue = Nothing
        , fieldType = FieldTypeParagraph
        , markupId = "description"
      }
      --Field {
          --label = "id"
        --, defaultValue = Nothing
        --, fieldType = FieldTypeText
        --, markupId = "id"
      --}
    ]
  }


