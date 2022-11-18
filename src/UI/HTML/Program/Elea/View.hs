--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Program.Elea.View (
    html
  ) where


import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import qualified UI.HTML.Comp.StateButton as StateButton
import qualified UI.HTML.Comp.Link as Link


-- | HTML for Home state
html :: Assets -> Html
html assets = do
  H.div ! classes ["elea-view"] $ do
    return ()

-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "elea-view-" <> s 
