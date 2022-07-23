--
-- HTML.Comp.Form
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.HTML.Comp.Form (
    html
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze (toMarkup, toValue)

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)
import Web.Types.Form (
    Form
  , Field, FieldType (..)
  )


-- | View HTML
html :: Assets -> Form -> Html
html (Assets iconIndex) form = do
  H.div ! A.class_ "comp-form" $ do
    H.div ! A.class_ "comp-form-content" $ do
      H.form $ do
        forM_ form.fields fieldHtml


-- | Field HTML
fieldHtml :: Field -> Html
fieldHtml field = case field.fieldType of 
  FieldTypeText      -> fieldTextHtml field
  FieldTypeParagraph -> fieldParagraphHtml field


-- | Text input field HTML
fieldTextHtml :: Field -> Html
fieldTextHtml field = do
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue $ field.markupId) $ 
      toMarkup field.label
    H.input ! A.type_ (toValue $ field.markupId) 
            ! A.name (toValue $ field.markupId)


-- | Text Area field HTML
fieldParagraphHtml :: Field -> Html
fieldParagraphHtml field = do
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue $ field.markupId) $ 
      toMarkup field.label
    H.textarea ! A.type_ (toValue $ field.markupId) 
               ! A.name (toValue $ field.markupId) $
      return ()


