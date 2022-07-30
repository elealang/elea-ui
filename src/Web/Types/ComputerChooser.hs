--
-- WEB: Types.ComputerChooser
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types.ComputerChooser (
    -- Public types
    ComputerChooser (..)
  , State (..)
    -- Constructors
  , new
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
import           Data.Text (Text)
import           GHC.Generics

import           Elea.Base (
    Computer (..), existence
  , Object (..)
  )
import           Elea.Index (ComputerIndex)


-- | Computer Chooser
data ComputerChooser = ComputerChooser {
    state            :: State
  , selectedComputer :: Computer
  , computerIndex    :: ComputerIndex
  , object           :: Object
}

instance ToJSON ComputerChooser where
  toJSON computerChooser = JSON.object [
      "state"             .= computerChooser.state
    , "object"            .= computerChooser.object
    , "selected_computer" .= computerChooser.selectedComputer
    ]


-- | Create a new Computer Chooser with default values
new :: ComputerIndex -> Object -> ComputerChooser
new compIdx obj = ComputerChooser List existence compIdx obj


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

