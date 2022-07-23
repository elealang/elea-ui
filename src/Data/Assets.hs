--
-- Assets
--

module Data.Assets where


import Data.Icon (IconIndex)


data Assets = Assets {
  iconIndex :: IconIndex
} deriving (Eq, Show)

