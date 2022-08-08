--
-- HTML.Comp.ArrowEditor
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.ArrowEditor (
    html
  ) where

import Control.Monad (forM_)
import qualified Data.List as L (intercalate)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze (toValue)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import UI.Data.Assets (Assets (..))
import UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Comp.Form as Form
import UI.Types.Form (
    Form (..)
  , Field (..), FieldBasic (..), FieldChoice (..)
  , FieldType (..)
  )
import qualified UI.Types.Form as Form (
    textField, paragraphField
  , divider
  )


-- | View HTML
html :: Assets -> Html
html assets = do
  H.div ! A.class_ "comp-arrow-editor" $ do
    H.div ! A.class_ "comp-arrow-editor-content comp" $ do
      H.div ! classes [cls "header", "comp-header"] $ do
        H.div ! classes [cls "title", "comp-header-title"] $ "ARROW"
        H.div ! classes [cls "subtitle", "comp-header-subtitle"] $ "RELATIONSHIP"
      H.div ! classes [cls "pane", "is-pane"] $ do
        H.div ! A.class_ "comp-story-editor-form" $ do
          Form.html assets form
  
--id: "Hello, World!"
--app-id: hello-world
--init-state: *
--term-state: @


-- | Form HTML
form :: Form
form = Form {
    fields = [
      Basic $ FieldBasic {
          label         = "name"
        , defaultValue  = Nothing
        , fieldType     = FieldTypeText
        , markupId      = "name"
        , isModel       = True
        , valueFunction = Nothing
      }
    , Basic $ FieldBasic {
          label         = "description"
        , defaultValue  = Nothing
        , fieldType     = FieldTypeParagraph
        , markupId      = "description"
        , isModel       = False
        , valueFunction = Nothing
      }
    , Form.divider
    , Basic $ FieldBasic {
          label         = "application id"
        , defaultValue  = Nothing
        , fieldType     = FieldTypeAppId
        , markupId      = "app-id"
        , isModel       = False
        , valueFunction = Just jsKebabCaseFunction
      }
    , Form.divider
    , Choice $ FieldChoice "start state"[
        ( "any"
        , Form.textField "start state" "init-state"
        )
      , ( "id"
        , Form.textField "end state" "term-state"
        )
      ]
    , Choice $ FieldChoice "end state"[
        ( "mind"
        , Form.textField "start state" "init-state"
        )
      , ( "id"
        , Form.textField "end state" "term-state"
        )
      ]
    ]
  }


jsKebabCaseFunction :: Text
jsKebabCaseFunction = "name.toLowerCase().replace(' ', '-').replace(/[^a-z0-9-]+/gi, '')"


-- | HTML helper combinators 
classes l = A.class_ $ toValue $ L.intercalate " " l
cls s = "comp-arrow-editor-" <> s 
