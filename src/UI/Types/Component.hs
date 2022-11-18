--
-- WEB: Types
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module UI.Types.Component (
    Component (..)
  ) where


import           Data.Aeson (ToJSON)


-- | IsComponent
class (ToJSON b) => Component a b where
  toComponentData  :: a -> b

