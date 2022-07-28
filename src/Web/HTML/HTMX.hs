--
-- HTML: HTMX
--

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.HTML.HTMX (
    get, target
  ) where


import Data.Text (Text)
import qualified Data.Text as T (toLower)
import Text.Blaze (
    Attribute, toValue
  , customAttribute
  )
import Text.Blaze.Html (Html)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


get :: Text -> Attribute
get route = customAttribute "hx-get" $ toValue route  

target :: Text -> Attribute
target tag = customAttribute "hx-target" $ toValue tag  
