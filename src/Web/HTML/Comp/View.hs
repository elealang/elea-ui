--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.View (
    html
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze (preEscapedText)

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)
import qualified Web.HTML.Comp.ComputerChooser as ComputerChooser
import qualified Web.HTML.Comp.ProgramEditor as ProgramEditor
import qualified Web.HTML.Comp.StoryEditor as StoryEditor
import Web.Types.View (
    View
  , ViewType (..)
  )
import qualified Web.Types.ComputerChooser as ComputerChooser


-- | View HTML
html :: Assets -> View -> Html
html assets view = do
  H.div ! A.class_ "comp-view" $ do
    H.div ! A.class_ "comp-view-main" $ do
      sidebarHTML "edit" assets
      mainCompHTML
    H.div ! A.class_ "comp-view-next" $ do
      sidebarHTML "arrow-right" assets
      ComputerChooser.html ComputerChooser.new assets
  where
    mainCompHTML = case view.typ of
      DefineStory   -> StoryEditor.html assets
      DefineProgram -> ProgramEditor.html assets


sidebarHTML :: Text -> Assets -> Html
sidebarHTML iconName (Assets iconIndex) = do
  H.div ! A.class_ "comp-view-sidebar" $
    H.div ! A.class_ "comp-view-sidebar-icon" $
      H.preEscapedText $ fromJust $ iconSVGWithName iconName iconIndex
