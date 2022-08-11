--
-- WEB: Types.EleaState
--

{-# LANGUAGE DeriveGeneric #-}

module UI.Types.Elea (
    EleaState (..)
  , stateVerb, stateObject
  , stateRoute
  , View (..), newView
  , arrow
  ) where


import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  )
import Data.Text (Text)
import GHC.Generics

import Elea.Base (
    Arrow (..), ArrowId (..), ArrowName (..)
  , StateId (..)
  )


data EleaState = 
    GeneralHome
  | EleaComputeStory
  | EleaCreateStory
  | EleaEditStory
  | EleaCreateProgram
  | EleaCreateAbstraction
  | EleaCreateState
  | EleaCreateArrow
  | IndexFindStory
  | IndexFindProgram
  | IndexFindAbstraction
  | CollectServers
  | CollectStories
  | CollectComputers
  | SortStories
  deriving (Generic, Show)


instance ToJSON EleaState where
  toEncoding = genericToEncoding defaultOptions


-- | EleaState stateVerb
stateVerb :: EleaState -> Text
stateVerb GeneralHome           = "view"
stateVerb EleaComputeStory      = "compute"
stateVerb EleaCreateStory       = "create"
stateVerb EleaEditStory         = "edit"
stateVerb EleaCreateProgram     = "create"
stateVerb EleaCreateAbstraction = "create"
stateVerb EleaCreateState       = "create"
stateVerb EleaCreateArrow       = "create"
stateVerb IndexFindStory        = "find"
stateVerb IndexFindProgram      = "find"
stateVerb IndexFindAbstraction  = "find"
stateVerb CollectServers        = "collect"
stateVerb CollectStories        = "collect"
stateVerb CollectComputers      = "collect"
stateVerb SortStories           = "sort"


-- | EleaState stateObject
stateObject :: EleaState -> Text
stateObject GeneralHome           = "home"
stateObject EleaComputeStory      = "story"
stateObject EleaCreateStory       = "story"
stateObject EleaEditStory         = "story"
stateObject EleaCreateProgram     = "program"
stateObject EleaCreateAbstraction = "abstraction"
stateObject EleaCreateState       = "state"
stateObject EleaCreateArrow       = "arrow"
stateObject IndexFindStory        = "story"
stateObject IndexFindProgram      = "program"
stateObject IndexFindAbstraction  = "abstraction"
stateObject CollectServers        = "servers"
stateObject CollectStories        = "stories"
stateObject CollectComputers      = "computers"
stateObject SortStories           = "stories"


stateRoute :: EleaState -> Text
stateRoute GeneralHome           = "/"
stateRoute EleaCreateStory       = "/create-story"
stateRoute EleaCreateProgram     = "/create-program"
stateRoute EleaCreateAbstraction = "/create-abstraction"
stateRoute EleaCreateState       = "/create-state"
stateRoute EleaCreateArrow       = "/create-arrow"
stateRoute IndexFindStory        = "/find-story"
stateRoute IndexFindProgram      = "/find-program"
stateRoute IndexFindAbstraction  = "/find-abstraction"
stateRoute CollectServers        = "/collect-servers"
stateRoute CollectStories        = "/collect-stories"
stateRoute CollectComputers      = "/collect-computers"
stateRoute SortStories           = "/sort-stories"
stateRoute _                     = "/"


arrow :: EleaState -> Arrow
arrow GeneralHome = Arrow {
    id        = ArrowId "view-home"
  , name      = ArrowName "View Home"
  , initState = StateId "undefined"
  , termState = StateId "undefined"
  , mutations = []
}
arrow _           = Arrow {
    id        = ArrowId "undefined"
  , name      = ArrowName "Undefined"
  , initState = StateId "undefined"
  , termState = StateId "undefined"
  , mutations = []
}
--arrow EleaComputeStory      = "story"
--arrow EleaCreateStory       = "story"
--arrow EleaEditStory         = "story"
--arrow EleaCreateProgram     = "program"
--arrow EleaCreateAbstraction = "abstraction"
--arrow EleaCreateState       = "state"
--arrow EleaCreateArrow       = "arrow"
--arrow IndexFindStory        = "story"
--arrow IndexFindProgram      = "program"
--arrow IndexFindAbstraction  = "abstraction"
--arrow CollectServers        = "servers"
--arrow CollectStories        = "stories"
--arrow CollectComputers      = "computers"
--arrow SortStories           = "stories"


-- | View
newtype View = View {
    chooserOpen :: Bool
} deriving (Generic, Show)

instance ToJSON View where
  toEncoding = genericToEncoding defaultOptions


-- | New View
newView :: View
newView = View False

