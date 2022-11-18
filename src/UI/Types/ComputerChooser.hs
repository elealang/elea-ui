--
-- WEB: Types.ComputerChooser
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module UI.Types.ComputerChooser (
    -- Public types
    ComputerChooser (..)
  , State (..)
    -- Constructors
  -- , new
  ) where


import           Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  , Options (constructorTagModifier)
  , (.=)
  )
import qualified Data.Aeson as JSON (
    Value (..)
  , object
  )
import qualified Data.Text as T (pack, toLower)
import           GHC.Generics

--import           Elea.Base (
    --Computer (..), existence
  --, Def (..)
  --)
--import           Elea.Server (Server)


-- | Computer Chooser
data ComputerChooser = ComputerChooser {
    state            :: State
  --, selectedComputer :: Computer
  --, servers          :: [Server]
  -- , object           :: Def
}

instance ToJSON ComputerChooser where
  toJSON computerChooser = JSON.object [
      "state"             .= computerChooser.state
   -- , "object"            .= computerChooser.object
    -- , "selected_computer" .= computerChooser.selectedComputer
    ]


-- | Create a new Computer Chooser with default values
--new :: [Server] -> Def -> ComputerChooser
--new = ComputerChooser List existence


-- | State
data State = 
    List
  | View
  | Search
  deriving (Generic, Eq, Ord, Show)

instance ToJSON State where
  toJSON state = JSON.String $ T.toLower $ T.pack $ show state
  --toEncoding = genericToEncoding $ defaultOptions {
    --constructorTagModifier = map C.toLower 
  --}

