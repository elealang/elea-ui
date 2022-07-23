--
-- WEB: Types.ComputerChooser
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types.ComputerChooser (
    -- Public types
    ComputerChooser (..), State (..)
    -- Constructors
  , new
  ) where

import Data.Aeson (
    ToJSON (..)
  , genericToEncoding, defaultOptions
  , Options (constructorTagModifier)
  )
import qualified Data.Char as C (toLower)
import Data.Text (Text)
import GHC.Generics


-- | Computer Chooser
data ComputerChooser = ComputerChooser {
  state :: State
} deriving (Generic, Show)

instance ToJSON ComputerChooser where
  toEncoding = genericToEncoding defaultOptions


-- | Create a new Computer Chooser with default values
new :: ComputerChooser
new = ComputerChooser View


-- | State
data State = 
    List
  | View
  | Search
  deriving (Generic, Eq, Ord, Show)

instance ToJSON State where
  toEncoding = genericToEncoding $ defaultOptions {
    constructorTagModifier = map C.toLower 
  }
