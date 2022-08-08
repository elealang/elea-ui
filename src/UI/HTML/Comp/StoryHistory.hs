--
-- HTML.Comp.ComputerChooser
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.StoryHistory (
    html
  ) where


import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))

import Elea.Base (Story (..))


-- | Story History HTML 
html :: Maybe Story -> Assets -> Html
html _ _ = do
  H.div ! A.class_ "comp-story-history" $ do
    return ()

-- | HTML helper combinators 
--classes :: [String] -> H.Attribute
--classes l = A.class_ $ H.toValue $ unwords l

--cls :: String -> String
--cls s = "comp-story-history-" <> s 
