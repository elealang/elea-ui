--
-- HTML.Comp.View
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.Home (
    html
  ) where


import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import qualified UI.HTML.Comp.StateButton as StateButton
import qualified UI.HTML.Comp.Link as StoryLink
import           UI.Types.Elea as Elea (EleaState (..))


html :: Assets -> Html
html assets = do
  H.div ! A.class_ "comp-home" $ do
    H.div ! A.class_ "comp-home-main" $ do
      H.div ! A.class_ "comp-home-main-text" $
        "Tell Your Story."
      H.div ! A.class_ "comp-home-main-examples" $ do
        StoryLink.html "Say Hello, World!" "emoji-wave" assets
        StoryLink.html "Program Your Mind" "emoji-meditation" assets
        StoryLink.html "Play a Game with Strangers" "emoji-video-game" assets
        StoryLink.html "Read/Write a Book" "emoji-book" assets
        StoryLink.html "Share Ideas with the World" "emoji-thoughts" assets
        StoryLink.html "Manage Your Finances" "emoji-finance" assets
        StoryLink.html "Buy Stuff Online" "emoji-buy" assets
        StoryLink.html "Practice a Religion" "emoji-religion" assets
        StoryLink.html "Analyze Data" "emoji-technology" assets
        StoryLink.html "Learn New Skills" "emoji-learn" assets
      H.div ! A.class_ "comp-home-main-actions" $ do
        H.div ! A.class_ "comp-home-main-actions-buttons" $ do
          StateButton.html StateButton.Inline Elea.EleaCreateStory assets
          StateButton.html StateButton.Inline Elea.IndexFindStory assets
    H.div ! A.class_ "comp-home-help" $ return ()
