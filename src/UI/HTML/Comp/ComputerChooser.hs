--
-- HTML.Comp.ComputerChooser
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.ComputerChooser (
    html
  ) where


import           Control.Monad (forM_)
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import           Data.Maybe (fromJust)
import qualified Data.Text as T (pack)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Data.Icon (iconSVGWithName)
import qualified UI.HTML.Alpine as X
import qualified UI.HTML.Comp.StateButton as StateButton (html, ButtonType (..))
-- import           UI.Types.Elea as Elea
import           UI.Types.ComputerChooser (ComputerChooser)

-- import           Elea.Server (Server)


-- | Computer Chooser HTML
html :: ComputerChooser -> Assets -> Html
html chooser assets = do
  X.html chooser "comp-computer-chooser" $ do
    H.div ! A.class_ ((cls "pane") <> " is-pane") $ do
      -- listStateHTML chooser.servers assets
      listStateHTML assets
      viewStateHTML assets
  -- where
  --   eleaState _        = Elea.EleaComputeStory 
 

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
    H.div ! A.class_ (cls "pane-content") $
      H.div ! A.class_ (cls "search-result-list") $ return ()
        --forM_ servers $ computerHTML assets
  --where
    --computerHTML _assets server = do
      --let computerJSON = T.pack $ LBS.unpack $ encode server
      --H.div ! A.class_ (cls "search-result")
            -- ! X.onClick ("state = 'view'; selected_computer = " <> computerJSON) $ do
        --H.div ! A.class_ (cls "search-result-info") $ do
          --H.div ! A.class_ (cls "search-result-name") $ H.toMarkup server.name.getServerName
          --H.div ! A.class_ (cls "search-result-domain") $ 
            --H.toMarkup server.uri
        --H.div ! A.class_ (cls "search-result-icon") $ do
          --H.preEscapedText $ fromJust $ iconSVGWithName "compute" assets.iconIndex

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
classes l = A.class_ $ H.toValue $ unwords l
cls s = "comp-computer-chooser-" <> s 

