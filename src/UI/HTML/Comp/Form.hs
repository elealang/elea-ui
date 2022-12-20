--
-- HTML.Comp.Form
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module UI.HTML.Comp.Form (
    html
  ) where


import Control.Monad (forM_)
import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  )
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze (toMarkup, toValue)

import UI.Data.Assets (Assets (..))
import qualified UI.HTML.Alpine as X
import UI.Data.Icon (iconSVGWithName)
import UI.Types.Form (
    Form
  , Field (..), FieldBasic (..), FieldChoice (..)
  , FieldType (..)
  )


-- | View HTML
html :: Assets -> Form -> Html
html assets _form = do
  X.html _form "comp-form" $ do
    H.div ! A.class_ "comp-form-content" $ do
      H.form $ do
        forM_ _form.fields $ fieldHtml assets


-- | Field HTML
fieldHtml :: Assets -> Field -> Html
fieldHtml assets (Basic field                 ) = 
  case field.fieldType of 
    FieldTypeText      -> textHtml field
    FieldTypeParagraph -> paragraphHtml field
    FieldTypeAppId     -> appIdHTML field assets
    FieldTypeDivider   -> dividerHTML
fieldHtml _      (Choice fieldChoice) = choiceHTML fieldChoice
fieldHtml assets (Line   fields     ) = 
  H.div ! A.class_ "comp-form-line" $ 
    forM_ fields (fieldHtml assets)


-- | Text input field HTML
textHtml :: FieldBasic -> Html
textHtml field = do
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue field.markupId) $ 
      toMarkup field.label
    H.div ! A.class_ "comp-form-field-input" $
      let input   = H.input ! A.type_ (toValue field.markupId)
                            ! A.name (toValue field.markupId)
          input'  = case field.isModel of
                      True  -> input ! X.model field.markupId
                      False -> input
          input''  = case field.defaultValue of
                       Just t  -> input ! A.value (H.toValue t)
                       Nothing -> input'
          --input'' = case field.valueFunction of
                      --Just fn -> input ! X.text fn
                      --Nothing -> input'
      in  input''


-- | Text Area field HTML
paragraphHtml :: FieldBasic -> Html
paragraphHtml field = do
  H.div ! A.class_ "comp-form-field-text-area comp-form-field" $ do
    H.textarea ! A.type_ (toValue field.markupId) 
               ! A.name (toValue field.markupId) $
      case field.defaultValue of
        Just t  -> H.toMarkup t
        Nothing -> H.toMarkup ("nohing" :: Text)
    H.label ! A.for (toValue field.markupId) $ 
      toMarkup field.label


-- | Application ID field HTML
appIdHTML :: FieldBasic -> Assets -> Html
appIdHTML field (Assets iconIndex)= do
  H.div ! A.class_ "comp-form-field" $ do
    H.label ! A.for (toValue field.markupId) $ 
      toMarkup field.label
    H.div ! A.class_ "comp-form-field-input" $
      let input  = H.input ! A.type_ (toValue field.markupId)
                           ! A.name (toValue field.markupId)
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
          
