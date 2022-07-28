--
-- HTML.Comp.Form
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.HTML.Comp.Form (
    html
  ) where

import Control.Monad (forM_)
import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  )
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (empty)
import GHC.Generics
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
  , Field (..), FieldBasic (..), FieldChoice (..)
  , FieldType (..)
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
fieldHtml assets (Basic field                 ) = 
  case field.fieldType of 
    FieldTypeText      -> textHtml field
    FieldTypeParagraph -> paragraphHtml field
    FieldTypeAppId     -> appIdHTML field assets
    FieldTypeDivider   -> dividerHTML
fieldHtml assets (Choice fieldChoice) = choiceHTML fieldChoice


-- | Text input field HTML
textHtml :: FieldBasic -> Html
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
paragraphHtml :: FieldBasic -> Html
paragraphHtml field = do
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue $ field.markupId) $ 
      toMarkup field.label
    H.textarea ! A.type_ (toValue $ field.markupId) 
               ! A.name (toValue $ field.markupId) $
      return ()


-- | Application ID field HTML
appIdHTML :: FieldBasic -> Assets -> Html
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


dividerHTML :: Html
dividerHTML = H.div ! A.class_ "comp-form-field-divider" $ return ()



data ChoiceData = ChoiceData {
    choice  :: Text
  , choices :: [Text]  
} deriving (Generic, Show)

instance ToJSON ChoiceData where
  toEncoding = genericToEncoding defaultOptions


choiceHTML :: FieldChoice -> Html
choiceHTML (FieldChoice label choices) = do
  let choiceNames = fst <$> choices
      choiceData = ChoiceData {
        choice  = head choiceNames
      , choices = choiceNames
      } 
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue label) $ toMarkup label
    H.div ! A.class_ "comp-form-field-input" $
      X.html choiceData "comp-form-field-choice" $ 
        forM_ choices caseHTML 
  where
    caseHTML (name, field) = do 
      H.div ! A.class_ "comp-form-field-choice-case" 
            ! X.show_ ("choice == '" <> name <> "'") $ do 
        H.div ! A.class_ "comp-form-field-choice-case-button" $
          toMarkup  name
        H.div ! A.class_ "comp-form-field-choice-case-field" $ return ()
          
