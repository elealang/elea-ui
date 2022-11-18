--
-- Story Editor HTML Component
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.StoryEditor (
    html
  ) where


import           Control.Monad (forM_)
import           Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import qualified UI.HTML.Comp.Form as Form (html)
import qualified UI.HTML.Comp.StateButton as StateButton (html, ButtonType (..))
-- import           UI.Types.Elea as Elea
import           UI.Types.Form (
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
  H.div ! A.class_ "comp-story-editor" $ do
    H.div ! A.class_ "comp-story-editor-content" $ do
      H.div ! A.class_ "comp-story-editor-pane is-pane" $ do
        H.div ! A.class_ "comp-story-editor-form" $ do
          Form.html assets form
  

form :: Form
form = Form {
    fields = [
        Basic $ Form.textField "name" "name"
      , Basic $ Form.paragraphField "description" "description"
    ]
  }


