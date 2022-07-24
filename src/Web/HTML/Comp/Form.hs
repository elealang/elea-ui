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
import qualified Web.HTML.Alpine as X
import Data.Icon (iconSVGWithName)
import Web.Types.Form (
    Form
  , Field, FieldType (..)
  )


-- | View HTML
html :: Assets -> Form -> Html
html assets form = do
  X.html form "comp-form" $ do
    H.div ! A.class_ "comp-form-content" $ do
      H.form $ do
        forM_ form.fields $ fieldHtml assets


-- | Field HTML
fieldHtml :: Assets -> Field -> Html
fieldHtml assets field = case field.fieldType of 
  FieldTypeText      -> textHtml field
  FieldTypeParagraph -> paragraphHtml field
  FieldTypeAppId     -> appIdHTML field assets


-- | Text input field HTML
textHtml :: Field -> Html
textHtml field = do
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue $ field.markupId) $ 
      toMarkup field.label
    H.div ! A.class_ "comp-form-field-input" $
      let input   = H.input ! A.type_ (toValue $ field.markupId)
                            ! A.name (toValue $ field.markupId)
          input'  = case field.isModel of
                      True  -> input ! X.model field.markupId
                      False -> input
          input'' = case field.valueFunction of
                      Just fn -> input ! X.text fn
                      Nothing -> input'
      in  input'



-- | Text Area field HTML
paragraphHtml :: Field -> Html
paragraphHtml field = do
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue $ field.markupId) $ 
      toMarkup field.label
    H.textarea ! A.type_ (toValue $ field.markupId) 
               ! A.name (toValue $ field.markupId) $
      return ()

-- | Application ID field HTML
appIdHTML :: Field -> Assets -> Html
appIdHTML field (Assets iconIndex)= do
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue $ field.markupId) $ 
      toMarkup field.label
    H.div ! A.class_ "comp-form-field-input" $
      let input  = H.input ! A.type_ (toValue $ field.markupId)
                           ! A.name (toValue $ field.markupId)
          input' = case field.valueFunction of
                     Just fn -> input ! X.value fn
                     Nothing -> input'
      in  do
        input'
        H.div ! A.class_ "comp-form-field-app-id-refresh-button" $
          H.preEscapedText $ fromJust $ iconSVGWithName "sync" iconIndex


