--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Program.Elea.ViewBasic.ObjectPane (
    html
  ) where


import           Control.Monad (mapM, forM_)
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.HashMap.Strict as HM (elems)
import           Data.Text (Text)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.Types.Form as Form
import qualified UI.HTML.Comp.Form as Form
import qualified UI.HTML.Comp.Link as Link
import           UI.Types.Page (Page)

import qualified Elea

import Debug.Trace (traceShow)


-- | HTML for Home state
html :: Page -> Elea.Program' -> Elea.ObjectRef -> Html
html page program' objectRef = do
  H.div ! classes ["program-elea-program-view_basic-object-pane"] $ do
    case mObject of
      Just object -> case object of
        Elea.ObjectAbstraction abstraction -> abstractionHTML abstraction page.assets
        Elea.ObjectState       state       -> stateHTML state page.assets
        Elea.ObjectArrow       arrow       -> return ()
        Elea.ObjectComputer    computer    -> return ()
        Elea.ObjectType        type_       -> return ()
        Elea.ObjectMutation    mutation    -> return ()
        Elea.ObjectProperty    property    -> return ()
        Elea.ObjectAgent       agent       -> return ()
        Elea.ObjectStory       story       -> return ()
        Elea.ObjectEvent       event       -> return ()
      Nothing    -> 
        H.div ! classes [cls "not-found"] $ return ()
  where
    mObject = Elea.object program' objectRef

-- | Abstraction HTML
abstractionHTML :: Elea.Abstraction -> Assets -> Html
abstractionHTML abstraction assets = do
  H.div ! classes [cls "main"] $ do
    H.div ! classes [cls "group"] $ do
      H.div ! classes [cls "group-header"] $
        "Metadata"
      H.div ! classes [cls "group-content"] $
        Form.html assets form
    H.div ! classes [cls "group"] $ do
      H.div ! classes [cls "group-header"] $
        "States"
      H.div ! classes [cls "group-content"] $ return ()
    H.div ! classes [cls "group"] $ do
      H.div ! classes [cls "group-header"] $
        "Diagram"
      H.div ! classes [cls "group-content"] $ return ()
  H.div ! classes [cls "sidebar"] $ do
    H.div ! classes [cls "nav"] $ do
      H.div ! classes [cls "nav-button"] $ do
        "Metadata"
      H.div ! classes [cls "nav-button"] $ do
        "States"
      H.div ! classes [cls "nav-button"] $ do
        "Diagram"
  where
    absId = abstraction.id.value
    absName = fromMaybe (Elea.AbstractionName "") abstraction.name
    absDesc = fromMaybe (Elea.AbstractionDescription "") abstraction.description
    form = Form.Form [
        Form.Line [
          Form.Basic $ Form.textFieldWithDefault "Id" "id" absId
        , Form.Basic $ Form.textFieldWithDefault "Name" "name" absName.value
        ]
      , Form.Basic $ Form.paragraphFieldWithDefault "Description" "description" absDesc.value
      ]

-- | State HTML
stateHTML :: Elea.State -> Assets -> Html
stateHTML state assets = do
  H.div ! classes [cls "main"] $ do
    H.div ! classes [cls "group"] $ do
      H.div ! classes [cls "group-header"] $
        "Metadata"
      H.div ! classes [cls "group-content"] $
        Form.html assets form
    H.div ! classes [cls "group"] $ do
      H.div ! classes [cls "group-header"] $
        "Init Arrows"
      H.div ! classes [cls "group-content"] $ return ()
    H.div ! classes [cls "group"] $ do
      H.div ! classes [cls "group-header"] $
        "Term Arrows"
      H.div ! classes [cls "group-content"] $ return ()
    H.div ! classes [cls "group"] $ do
      H.div ! classes [cls "group-header"] $
        "Diagram"
      H.div ! classes [cls "group-content"] $ return ()
  H.div ! classes [cls "sidebar"] $ do
    H.div ! classes [cls "nav"] $ do
      H.div ! classes [cls "nav-button"] $ do
        "Metadata"
      H.div ! classes [cls "nav-button"] $ do
        "Init Arrows"
      H.div ! classes [cls "nav-button"] $ do
        "Term Arrows"
      H.div ! classes [cls "nav-button"] $ do
        "Diagram"
  where
    stId = state.id.value
    stName = fromMaybe (Elea.StateName "") state.name
    stDesc = fromMaybe (Elea.StateDescription "") state.description
    form = Form.Form [
        Form.Line [
          Form.Basic $ Form.textFieldWithDefault "Id" "id" stId
        , Form.Basic $ Form.textFieldWithDefault "Name" "name" stName.value
        ]
      , Form.Basic $ Form.paragraphFieldWithDefault "Description" "description" stDesc.value
      ]



-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "program-elea-program-view_basic-object-pane-" <> s 

