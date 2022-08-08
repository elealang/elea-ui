--
-- HTML: Alpine
--

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module UI.HTML.Alpine (
    html
  , show_, model
  -- Bindings
  , text, value
  -- Events
  , onClick, onClickOutside
  , onEnter, onLeave
  ) where

import Data.Aeson (
    ToJSON
  , encode
  )
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import Data.Text (Text)
import Text.Blaze (
    Attribute, toValue
  , customAttribute
  )
import Text.Blaze.Html (Html)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


html :: (ToJSON a) => a -> Text -> Html -> Html
html compValue compName innerHtml = do
  let compValueString = LBS.unpack $ encode compValue
  H.div ! A.class_ (toValue compName)
        ! customAttribute "x-data" (toValue compValueString) $ 
    innerHtml

show_ :: Text -> Attribute
show_ var = customAttribute "x-show" $ toValue var  

model :: Text -> Attribute
model var = customAttribute "x-model" $ toValue var  

text :: Text -> Attribute
text js = customAttribute "x-text" $ toValue js

value :: Text -> Attribute
value js = customAttribute "x-bind:value" $ toValue js

onClick :: Text -> Attribute
onClick js = customAttribute "@click" $ toValue js  

onClickOutside :: Text -> Attribute
onClickOutside js = customAttribute "@click.outside" $ toValue js  

onEnter :: Text -> Attribute
onEnter js = customAttribute "@pointerenter" $ toValue js  

onLeave :: Text -> Attribute
onLeave js = customAttribute "@pointerleave" $ toValue js  
