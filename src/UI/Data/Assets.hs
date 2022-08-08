--
-- Assets
--

module UI.Data.Assets where


import UI.Data.Icon (IconIndex)


data Assets = Assets {
  iconIndex :: IconIndex
} deriving (Eq, Show)

