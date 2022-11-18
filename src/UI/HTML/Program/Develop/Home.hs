--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Program.Develop.Home (
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
  H.div ! classes ["develop-home"] $ do
    H.div ! classes [cls "main"] $ do
      H.div ! classes [cls "main-text"] $
        "Tell Your Story."
      H.div ! classes [cls "main-examples"]  $ do
        Link.programHTML "hello-world" "Say Hello, World!" "emoji-wave" assets
        Link.programHTML "" "Program Your Mind" "emoji-meditation" assets
        Link.programHTML "" "Play a Game with Strangers" "emoji-video-game" assets
        Link.programHTML "" "Read/Write a Book" "emoji-book" assets
        Link.programHTML "" "Share Ideas with the World" "emoji-thoughts" assets
        Link.programHTML "" "Manage Your Finances" "emoji-finance" assets
        Link.programHTML "" "Buy Stuff Online" "emoji-buy" assets
        Link.programHTML "" "Practice a Religion" "emoji-religion" assets
        Link.programHTML "" "Analyze Data" "emoji-technology" assets
        Link.programHTML "" "Learn New Skills" "emoji-learn" assets
      H.div ! classes [cls "main-actions"]  $ do
        H.div ! classes [cls "main-actions-buttons"] $ return ()
          --StateButton.html StateButton.Inline Elea.EleaCreateStory assets
          --StateButton.html StateButton.Inline Elea.IndexFindStory assets
    H.div ! A.class_ "develop-home-help" $ return ()


-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes = A.class_ . H.toValue . unwords

cls :: String -> String
cls s = "develop-home-" <> s 
