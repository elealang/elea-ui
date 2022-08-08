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
import qualified UI.HTML.Comp.StoryLink as StoryLink
import           UI.Types.Elea as Elea (EleaState (..))


html :: Assets -> Html
html assets = do
  H.div ! A.class_ "comp-home" $ do
    H.div ! A.class_ "comp-home-main" $ do
      H.div ! A.class_ "comp-home-main-text" $
        "Tell Your Story."
      H.div ! A.class_ "comp-home-main-examples" $ do
        let linkSize = StoryLink.Large
        StoryLink.html "Building an App" linkSize assets
        StoryLink.html "Programing Your Mind" linkSize assets
        StoryLink.html "Playing a Game with Friends" linkSize assets
        StoryLink.html "Reading a Book" linkSize assets
        StoryLink.html "Analyzing the Economy " linkSize assets
        StoryLink.html "Finding a Hotel" linkSize assets
        StoryLink.html "Learning About Mathematics" linkSize assets
        StoryLink.html "Sharing Images with Friends" linkSize assets
        StoryLink.html "Taking out a Loan" linkSize assets
        StoryLink.html "Buying Stuff Online" linkSize assets
      H.div ! A.class_ "comp-home-main-actions" $ do
        H.div ! A.class_ "comp-home-main-actions-buttons" $ do
          StateButton.html StateButton.Inline Elea.EleaCreateStory assets
          StateButton.html StateButton.Inline Elea.IndexFindStory assets
    H.div ! A.class_ "comp-home-help" $ return ()
