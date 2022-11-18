--
-- HTML.Comp.EleaStateButton
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.StateButton (
    html
  , ButtonType (..)
  ) where


import qualified Data.Map as M (lookup, fromList)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Text.IO as T (putStrLn)
import qualified Data.Text as T (toLower, toUpper, unpack)

import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Alpine as X



data ButtonType = 
    Inline
  | Sidebar


-- | HTML
html :: ButtonType -> Assets -> Html
html buttonType assets = return ()
--case buttonType of
  --Inline  -> inlineHTML st assets
  --Sidebar -> sidebarHTML st assets
  

--inlineHTML :: EleaState -> Assets -> Html
--inlineHTML st assets = do
  --H.a ! classes ["comp-state-button", "type-inline", cls stateClass] 
      -- ! A.href (H.toValue $ Elea.stateRoute st) $ do
    --H.div ! classes [cls "content", "type-inline"] $ do
      --H.div ! classes [cls "icon", "type-inline"] $ iconByVerb (Elea.stateVerb st) assets
      --H.div ! classes [cls "verb", "type-inline"] $ H.toMarkup $ T.toUpper $ Elea.stateVerb st
      --H.div ! classes [cls "object", "type-inline"] $ H.toMarkup $ T.toUpper $ Elea.stateObject st
  --where
    --stateClass = T.unpack $ Elea.stateVerb st <> "-" <> Elea.stateObject st


--sidebarHTML :: EleaState -> Assets -> Html
--sidebarHTML st assets = do
  --let js = "modal_pane_open = true; "
        -- <> "previous_state_object = current_state_verb; "
        -- <> "previous_state_verb = current_state_object; "
        -- <> "current_state_verb = '" <> T.toUpper (Elea.stateVerb st) <> "'; " 
        -- <> "current_state_object = '" <> T.toUpper (Elea.stateObject st) <> "'; "
  --H.div ! classes ["comp-state-button", "type-sidebar"] 
        -- ! A.href (H.toValue $ Elea.stateRoute st)
        -- ! X.onClick js $
    --H.div ! classes [cls "content", "type-sidebar"] $ do
      --H.div ! classes [cls "icon", "type-inline"] $ iconByVerb (Elea.stateVerb st) assets
      --H.div ! classes [cls "verb", "type-sidebar"] $ H.toMarkup $ T.toUpper $ Elea.stateVerb st
      --H.div ! classes [cls "object", "type-sidebar"] $ H.toMarkup $ T.toUpper $ Elea.stateObject st


--iconByVerb :: Text -> Assets -> Html
--iconByVerb verb assets = do
  --let verb_ = T.toLower verb
      --iconName = fromJust $ M.lookup verb_ iconNameByVerb
  --H.preEscapedText $ fromJust $ iconSVGWithName iconName assets.iconIndex
  --where
    --iconNameByVerb = M.fromList [
        --("view", "arrow-right")
      --, ("compute", "compute")
      --, ("create", "plus")
      --, ("edit", "edit")
      --, ("find", "search")
      --, ("collect", "collect")
      --, ("sort", "sort")
      --]


--classes :: [String] -> H.Attribute
--classes l = A.class_ $ H.toValue $ unwords l

--cls :: String -> String
--cls s = "comp-state-button-" <> s 
