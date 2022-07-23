--
-- HTML.Comp.ProgramEditor
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.ProgramEditor (
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
  H.div ! A.class_ "comp-program-editor" $ do
    H.div ! A.class_ "comp-program-editor-content" $ do
      H.div ! A.class_ "comp-program-editor-header" $ do
        H.div ! A.class_ "comp-program-editor-title is-title" $ "PROGRAM"
        H.div ! A.class_ "comp-program-editor-subtitle is-subtitle" $ "INTENTION"
      H.div ! A.class_ "comp-program-editor-pane is-pane" $ do
        H.div ! A.class_ "comp-program-editor-form" $ do
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
        , fieldType = FieldTypeText
        , markupId = "description"
      }
    ]
  }


