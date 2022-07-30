--
-- HTML.Comp.ComputerChooser
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.ComputerChooser (
    html
  ) where


import           Control.Monad (forM_)
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import qualified Data.Char as C (toUpper)
import           Data.Maybe (fromJust)
import qualified Data.List as L (intercalate)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (putStrLn)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Data.Icon (iconSVGWithName)
import qualified Web.HTML.Alpine as X
import           Web.Types.ComputerChooser (ComputerChooser)
import qualified Web.Types.ComputerChooser as ComputerChooser

import           Elea.Base (Computer, Object)


-- | Computer Chooser HTML
html :: ComputerChooser -> Assets -> Html
html chooser assets = do
  X.html chooser "comp-computer-chooser" $ do
    H.div ! classes [(cls "header"), "comp-header"] $ do
      H.div ! A.class_ "comp-computer-chooser-title comp-header-title" $ "COMPUTE"
      H.div ! A.class_ "comp-computer-chooser-subtitle comp-header-subtitle" $ "PROVE"
    H.div ! A.class_ ((cls "pane") <> " is-pane") $ do
      listStateHTML chooser.computerIndex.computers assets
      viewStateHTML assets

-- | List state HTML
listStateHTML :: [Computer] -> Assets -> Html 
listStateHTML computers assets = do
  H.div ! A.class_ ((cls "list") <> " is-pane-state")
        ! X.show_ "state == 'list'" $ do
    H.div ! A.class_ (cls "pane-header") $ do
      H.div ! A.class_ (cls "state") $ do
        H.div ! A.class_ (cls "state-name") $ "find computers"
      H.div ! A.class_ (cls "search") 
            ! X.onClick "state = 'search'" $ do
        H.div ! A.class_ (cls "search-query") $ 
          H.div ! A.class_ (cls "search-subquery") $ "all"
        H.div ! A.class_ (cls "search-button") $ 
          H.preEscapedText $ fromJust $ iconSVGWithName "search" assets.iconIndex
    H.div ! A.class_ (cls "pane-content") $
      H.div ! A.class_ (cls "search-result-list") $
        forM_ computers $ computerHTML assets
  where
    computerHTML assets computer = do
      let computerJSON = T.pack $ LBS.unpack $ encode computer
      H.div ! A.class_ (cls "search-result")
            ! X.onClick ("state = 'view'; selected_computer = " <> computerJSON) $ do
        H.div ! A.class_ (cls "search-result-info") $ do
          H.div ! A.class_ (cls "search-result-name") $ H.toMarkup computer.name
          H.div ! A.class_ (cls "search-result-domain") $ 
            H.toMarkup computer.uri
        H.div ! A.class_ (cls "search-result-icon") $ do
          H.preEscapedText $ fromJust $ iconSVGWithName "computation" assets.iconIndex

-- | View state HTML
viewStateHTML :: Assets -> Html 
viewStateHTML assets = do
  H.div ! A.class_ ((cls "view") <> " is-pane-state")
        ! X.show_ "state == 'view'" $ do
    headerHTML
    contentHTML
  where
    headerHTML = do
      H.div ! A.class_ (cls "pane-header") $ do
        H.div ! A.class_ (cls "state") $ do
          H.div ! A.class_ (cls "state-name") $ "view computer"
        H.div ! A.class_ (cls "computer") $ do
          H.div ! A.class_ (cls "computer-name") 
                ! X.text "selected_computer.name" $ return ()
          H.div ! A.class_ (cls "back-button")
                ! X.onClick "state = 'list'" $ do
            H.preEscapedText $ fromJust $ iconSVGWithName "close" assets.iconIndex
    contentHTML = do
      H.div ! classes [cls "pane-content"] $ do
        H.div ! classes [cls "prop"] $ do
          H.div ! classes [cls "prop-label"] $ do
            H.div ! classes [cls "prop-label-tech"] $ "URI"
          H.div ! classes [(cls "prop-value"), "is-link"]
                ! X.text "selected_computer.uri" $ return ()
        H.div ! A.class_ (cls "prop") $ do
          H.div ! A.class_ (cls "prop-label") $ do
            H.div ! A.class_ (cls "prop-label-tech") $ "dependencies"
            H.div ! A.class_ (cls "prop-label-coll") $ "assumptions"
          H.div ! A.class_ (cls "prop-value") $ "No dependencies"
        H.div ! A.class_ (cls "prop") $ do
          H.div ! A.class_ (cls "prop-label") $ do
            H.div ! A.class_ (cls "prop-label-tech") $ "resource cost"
            H.div ! A.class_ (cls "prop-label-coll") $ "side-effects"
          H.div ! A.class_ (cls "prop-value") $ "Time"
          H.div ! A.class_ (cls "prop-value") $ "Money"
          H.div ! A.class_ (cls "prop-value") $ "Speed"
        H.div ! A.class_ (cls "footer") $ do
          H.div ! A.class_ (cls "compute-button") $ do
            H.div ! classes [cls "compute-button-verb"] $ "COMPUTE"
            H.div ! classes [cls "compute-button-object"] $ return ()
              -- H.toMarkup $ map C.toUpper $ show computer.forObject


-- | HTML helper combinators 
classes l = A.class_ $ H.toValue $ L.intercalate " " l
cls s = "comp-computer-chooser-" <> s 
