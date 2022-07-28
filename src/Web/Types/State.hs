--
-- WEB: Types.State
--

{-# LANGUAGE DeriveGeneric #-}

module Web.Types.State (
    State (..)
  , verb, object
  ) where


import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  )
import Data.Text (Text)
import GHC.Generics


data State = 
    MainCreateStory
  | StoryCreateProgram
  deriving (Generic, Show)


instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions


-- | State verb
verb :: State -> Text
verb MainCreateStory    = "create"
verb StoryCreateProgram = "create"


-- | State object
object :: State -> Text
object MainCreateStory    = "story"
object StoryCreateProgram = "program"
