--
-- HTML.Comp.ProgramEditor
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.ProgramEditor (
    html
  ) where

import Control.Monad (forM_)
import qualified Data.List as L (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze (preEscapedText, toValue)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import UI.Data.Assets (Assets (..))
import qualified UI.HTML.Comp.Form as Form
import UI.Types.Form (
    Form (..)
  , Field (..)
  )
import qualified UI.Types.Form as Form (
    textField
  , paragraphField
  )


-- | View HTML
html :: Assets -> Html
html assets = do
  H.div ! A.class_ "comp-program-editor" $ do
    H.div ! A.class_ "comp-program-editor-content comp" $ do
      H.div ! classes [cls "header", "comp-header"] $ do
        H.div ! classes [cls "title", "comp-header-title"] $ "PROGRAM"
        H.div ! classes [cls "subtitle", "comp-header-subtitle"] $ "INTENTION"
      H.div ! classes [cls "pane", "is-pane"] $ do
        H.div ! A.class_ "comp-program-editor-form" $ do
          Form.html assets form
  

form :: Form
form = Form {
    fields = [
      Basic $ Form.textField "name" "name"
    , Basic $ Form.paragraphField "description" "description"
    ]
  }


-- | HTML helper combinators 
classes l = A.class_ $ toValue $ L.intercalate " " l
cls s = "comp-program-editor-" <> s 
