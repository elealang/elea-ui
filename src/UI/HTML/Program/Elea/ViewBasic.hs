--
-- HTML.Comp.View
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Program.Elea.ViewBasic (
    -- Pages
    html
    -- Sections
  , objectsHTML
  , objectHTML
  ) where


import           Data.Aeson (
    ToJSON
  , toEncoding, genericToEncoding, defaultOptions
  )
import qualified Data.HashMap.Strict as HM (elems)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           GHC.Generics
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Alpine as X
import qualified UI.HTML.Comp.StateButton as StateButton
import qualified UI.HTML.Comp.Link as Link
import qualified UI.HTML.HTMX as HX
import qualified UI.HTML.Program.Elea.ViewBasic.FilterPane as FilterPane (html)
import qualified UI.HTML.Program.Elea.ViewBasic.ObjectPane as ObjectPane (html)
import           UI.Types.Page (Page)
import qualified UI.Temp as Temp

import qualified Elea


import Debug.Trace (traceShow)

--------------------------------------------------------------------------------
-- HTML
--------------------------------------------------------------------------------

-- HTML > Public
--------------------------------------------------------------------------------

-- | HTML for View Basic state
html :: Page -> Elea.Program -> Html
html page program = do
  let view = View "abstraction" False "" 
  X.html view "program-elea-program-view_basic" $ do
    headerHTML program page.assets
    -- use current program' as the default to display
    -- later will need to remember
    let program' = head $ HM.elems program.programById
    contentHTML page program'

-- | Program objects container html
objectsHTML :: Page -> Elea.Program' -> Elea.ObjectType -> Maybe Elea.ObjectRef -> Html
objectsHTML page program' objectType mObjectRef = do
  H.div ! classes [cls "filter-pane-container"] $
    FilterPane.html page program' objectType
  H.div ! classes [cls "object-pane-container"] $
    case mObjectRef of
      Just objectRef -> ObjectPane.html page program' objectRef
      Nothing        -> return ()

-- | Program objects container html
objectHTML :: Page -> Elea.Program' -> Elea.ObjectRef -> Html
objectHTML page program' objectRef =
  ObjectPane.html page program' objectRef

-- HTML > Private
--------------------------------------------------------------------------------

headerHTML :: Elea.Program -> Assets -> Html
headerHTML prog assets = do
  H.div ! classes [cls "header"] $ do
    H.div ! classes [cls "header-program-identity"] $ do
      H.div ! classes [cls "header-program-identity-name"] $ do
        H.toMarkup prog.identity.name.value
      -- Leave empty if no composition?
      H.div ! classes [cls "header-program-identity-divider"] $
        H.preEscapedText $ fromJust $ 
          iconSVGWithName "chevron-right" assets.iconIndex
      H.div ! classes [cls "header-program-identity-switcher"] $ "Main"
    H.div ! classes [cls "header-view"] $ do
      H.div ! classes [cls "header-view-tab", "is-active"] $
        H.div ! classes [cls "header-view-tab-text"] $ "Object"
      H.div ! classes [cls "header-view-tab"] $
        H.div ! classes [cls "header-view-tab-text"] $ "Project"
      H.div ! classes [cls "header-view-tab"] $
        H.div ! classes [cls "header-view-tab-text"] $ "Insight"
      H.div ! classes [cls "header-view-tab", cls "header-view-tab-add"] $
        H.preEscapedText $ fromJust $ 
          iconSVGWithName "plus-circle" assets.iconIndex

-- | Content
contentHTML :: Page -> Elea.Program' -> Html
contentHTML page prog = do
  H.div ! classes [cls "content"] $ do
    H.div ! classes [cls "program-pane-container"] $
      programPaneHTML page.assets
    H.div ! classes [cls "program-objects-container"] $ do
      let defaultObjectRef = Elea.ObjectRefAbstraction $ Elea.AbstractionId "main"
      objectsHTML page prog Elea.ObjectTypeAbstraction $ Just defaultObjectRef

-- | Program pane (tree/navigator)
programPaneHTML :: Assets -> Html
programPaneHTML assets = 
  H.div ! classes [cls "program-pane"] $ do
    spaceHTML
    timeHTML
    agencyHTML
    H.div ! classes [cls "program-pane-extra"] $ return ()
  where
    -- | Space
    spaceHTML = do
      H.div ! classes [cls "program-pane-space", cls "program-pane-group"] $ do
        H.div ! classes [cls "program-pane-space-label", cls "program-pane-label"] $ "Space"
        H.div ! classes [cls "program-pane-space-items"] $ do
          programPaneItemHTML "space" "abstraction" "Abstractions" True assets
          programPaneItemHTML "space" "state" "States" False assets
          programPaneItemHTML "space" "arrow" "Arrows" False assets
    -- | Time
    timeHTML = do
      H.div ! classes [cls "program-pane-time", cls "program-pane-group"] $ do
        H.div ! classes [cls "program-pane-time-label", cls "program-pane-label"] $ "Time"
        H.div ! classes [cls "program-pane-time-items"] $ do
          programPaneItemHTML "time" "type" "Types" False assets
          programPaneItemHTML "time" "computer" "Computers" False assets
          programPaneItemHTML "time" "mutation" "Mutations" False assets
          programPaneItemHTML "time" "effect" "Effects" False assets
          programPaneItemHTML "time" "property" "Properties" False assets
    -- | Agency
    agencyHTML = do
      H.div ! classes [cls "program-pane-agency", cls "program-pane-group"] $ do 
        H.div ! classes [cls "program-pane-agency-label", cls "program-pane-label"] $ "Agency"
        H.div ! classes [cls "program-pane-agency-items"] $ do 
          programPaneItemHTML "agency" "agent" "Agents" False assets
          programPaneItemHTML "agency" "story" "Stories" False assets
          programPaneItemHTML "agency" "event" "Events" False assets

programPaneItemHTML :: Text -> Text -> Text -> Bool -> Assets -> Html
programPaneItemHTML group objectType label isActive assets = do
  let className = T.unpack $ "program-pane-" <> group <> "-" <> objectType
      _classes  = [cls className, cls "program-pane-item"] 
                    -- <> (["is-active" | isActive]) 
  H.div ! classes _classes
        ! X.depClass ("activeObject == '" <> objectType <> "' ? 'is-active' : ''")
        ! X.depClass "activeObjectExpanded ? 'is-filtered' : ''"
        ! HX.get ("/view/elea/program/view-basic/program-objects?program-id=hello-world&object-type=" <> objectType)
        ! HX.target ".program-elea-program-view_basic-program-objects-container" 
        ! X.onClick onClickJS $ do
    H.div ! classes [cls "program-pane-item-header"] $ do
      H.div ! classes [cls "program-pane-item-header-label"] $
        H.div ! classes [] $
          H.toMarkup label
      H.div ! classes [cls "program-pane-item-header-icon"] $
        H.preEscapedText $ fromJust $ 
          iconSVGWithName "slider" assets.iconIndex
    H.div ! classes [cls "program-pane-item-filter"] $ return ()
        --H.preEscapedText $ fromJust $ 
          --iconSVGWithName "filter" assets.iconIndex
    H.div ! classes [cls "program-pane-item-sort"] $ return ()
        --H.preEscapedText $ fromJust $ 
          --iconSVGWithName "sort" assets.iconIndex
    where
      onClickJS = "if (activeObject === \"" <> objectType <> "\") {" 
        <> "    activeObjectExpanded = !activeObjectExpanded;"
        <> "} else {"
        <> "    activeObject = '" <> objectType <> "';"
        <> "}"

-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "program-elea-program-view_basic-" <> s 

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

data View = View {
    activeObject         :: Text
  , activeObjectExpanded :: Bool
  , other                :: Text
} deriving (Eq, Generic, Show)

instance ToJSON View where
    toEncoding = genericToEncoding defaultOptions


