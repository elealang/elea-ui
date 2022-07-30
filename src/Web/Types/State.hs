--
-- WEB: Types.State
--

{-# LANGUAGE DeriveGeneric #-}

module Web.Types.State (
    State (..)
  , verb, object
  , route
  ) where


import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  )
import Data.Text (Text)
import GHC.Generics


data State = 
    MainHome
  | MainCreateStory
  | MainFindStory
  | StoryCreateProgram
  deriving (Generic, Show)


instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions


-- | State verb
verb :: State -> Text
verb MainHome           = "go to"
verb MainCreateStory    = "create"
verb MainFindStory      = "find"
verb StoryCreateProgram = "create"


-- | State object
object :: State -> Text
object MainHome           = "home"
object MainCreateStory    = "story"
object MainFindStory      = "story"
object StoryCreateProgram = "program"


route :: State -> Text
route MainHome           = "/"
route MainCreateStory    = "/create-story"
route MainFindStory      = "/find-story"
route StoryCreateProgram = "/create-program"
