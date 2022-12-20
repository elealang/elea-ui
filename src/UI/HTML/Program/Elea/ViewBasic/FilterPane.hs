--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Program.Elea.ViewBasic.FilterPane (
    html
  ) where


import           Control.Monad (mapM, forM_)
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T (pack)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Comp.StateButton as StateButton
import qualified UI.HTML.Comp.Link as Link
import qualified UI.HTML.HTMX as HX
import           UI.Types.Page (Page)

import qualified Elea

import Debug.Trace (traceShow)


-- | HTML for Home state
html :: Page -> Elea.Program' -> Elea.ObjectType -> Html
html page program' objectType = do
  H.div ! classes ["program-elea-program-view_basic-filter-pane"] $
    H.div ! classes [cls "objects"] $ do
      forM_ (objectInfo program' objectType) $ 
        uncurry $ objectHTML objectType
      H.div ! classes [cls "object-fill"] $ return ()


-- | Get generic information about each Elea object to display 
-- as a list of object summaries/links in the filter pane
objectInfo :: Elea.Program' -> Elea.ObjectType -> [(Int, ObjectInfo)]
-- | Space/Abstraction info
objectInfo program' Elea.ObjectTypeAbstraction = 
  let info abs = 
        let name = fromMaybe (Elea.AbstractionName "") abs.name
            desc = fromMaybe (Elea.AbstractionDescription "") abs.description
        in ObjectInfo abs.id.value name.value desc.value
  in  zip [0..] $ info <$> program'.space.abstractions
-- | Space/State info
objectInfo program' Elea.ObjectTypeState       =  
  let info (_, st) = 
        let name = fromMaybe (Elea.StateName "") st.name
            desc = fromMaybe (Elea.StateDescription "") st.description
        in  ObjectInfo st.id.value name.value desc.value
  in  zip [0..] $ info <$> (Elea.spaceStates program'.space)
-- | Space/Arrow info
objectInfo program' Elea.ObjectTypeArrow       =  
  let info (_, arr) = 
        let name = fromMaybe (Elea.ArrowName "") arr.name
            desc = fromMaybe (Elea.ArrowDescription "") arr.description
        in  ObjectInfo arr.id.value name.value desc.value
  in  zip [0..] $ info <$> (Elea.spaceArrows program'.space)
-- | Time/Computer info
objectInfo program' Elea.ObjectTypeComputer    =  
  let info comp = 
        let name = fromMaybe (Elea.ComputerName "") comp.name
            desc = fromMaybe (Elea.ComputerDescription "") comp.description
        in  ObjectInfo comp.id.value name.value desc.value
  in  zip [0..] $ info <$> (program'.time.computers)
-- | Agency/Agent info
objectInfo program' Elea.ObjectTypeAgent    =  
  let info agent = 
        let name = fromMaybe (Elea.AgentName "") agent.name
            desc = fromMaybe (Elea.AgentDescription "") agent.description
        in  ObjectInfo agent.id.value name.value desc.value
  in  zip [0..] $ info <$> (program'.agency.agents)
objectInfo program' _                          = []



objectHTML :: Elea.ObjectType -> Int -> ObjectInfo -> Html
objectHTML objectType index (ObjectInfo _id _name _desc) = do
  let classes_ = ["program-elea-program-view_basic-filter-pane-object"]
                   <> (["is-active" | index == 0]) 
  H.div ! classes classes_ 
        ! HX.get (  "/view/elea/program/view-basic/program-object?program-id=hello-world&object-type=" 
                 <> (Elea.objectTypeAsText objectType)
                 <> "&object-id="
                 <> _id
                 )
        ! HX.target ".program-elea-program-view_basic-object-pane-container" $ do
    H.div ! classes ["program-elea-program-view_basic-filter-pane-object-header"] $ 
      H.toMarkup _name
    H.div ! classes ["program-elea-program-view_basic-filter-pane-object-main"] $ 
      H.toMarkup _desc
  


-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "program-elea-program-view_basic-filter-pane-" <> s 

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

data ObjectInfo = ObjectInfo {
    id   :: Text
  , name :: Text
  , desc :: Text
}
