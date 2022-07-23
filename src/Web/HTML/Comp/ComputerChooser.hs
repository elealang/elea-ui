--
-- HTML.Comp.ComputerChooser
--

{-# LANGUAGE OverloadedStrings #-}

module Web.HTML.Comp.ComputerChooser (
    html
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Data.List as L (intercalate)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze (preEscapedText, toValue)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)
import qualified Web.HTML.Alpine as X
import Web.Types.ComputerChooser (ComputerChooser)
import qualified Web.Types.ComputerChooser as ComputerChooser


-- | Computer Chooser HTML
html :: ComputerChooser -> Assets -> Html
html chooser assets = do
  X.html chooser "comp-computer-chooser" $ do
    H.div ! classes [(cls "header"), "comp-header"] $ do
      H.div ! A.class_ "comp-computer-chooser-title comp-header-title" $ "COMPUTE"
      H.div ! A.class_ "comp-computer-chooser-subtitle comp-header-subtitle" $ "PROVE"
    H.div ! A.class_ ((cls "pane") <> " is-pane") $ do
      listStateHTML assets
      viewStateHTML assets

-- | List state HTML
listStateHTML :: Assets -> Html 
listStateHTML assets = do
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
    H.div ! A.class_ (cls "pane-content") $ do
      H.div ! A.class_ (cls "search-result-list") $ do
        H.div ! A.class_ (cls "search-result")
              ! X.onClick "state = 'view'" $ do
          H.div ! A.class_ (cls "search-result-info") $ do
            H.div ! A.class_ (cls "search-result-name") $ "Elea World"
            H.div ! A.class_ (cls "search-result-domain") $ "world.elea.computer"
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
          H.div ! A.class_ (cls "computer-name") $ "Elea World"
          H.div ! A.class_ (cls "back-button") $ 
            H.preEscapedText $ fromJust $ iconSVGWithName "close" assets.iconIndex
    contentHTML = do
      H.div ! A.class_ (cls "pane-content") $ do
        H.div ! A.class_ (cls "prop") $ do
          H.div ! A.class_ (cls "prop-label") $ do
            H.div ! classes [cls "prop-label-tech"] $ "domain"
          H.div ! classes [(cls "prop-value"), "is-link"] $ "world.elea.computer"
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
            H.div ! A.class_ (cls "compute-button-verb") $ "COMPUTE"
            H.div ! A.class_ (cls "compute-button-object") $ "STORY"


-- | HTML helper combinators 
classes l = A.class_ $ toValue $ L.intercalate " " l
cls s = "comp-computer-chooser-" <> s 