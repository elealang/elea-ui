--
-- Story Browser HTML Component
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.SetBuilder (
    html
  ) where


import           Control.Monad (when)
import           Data.Maybe (fromJust)
import qualified Data.Text as T (pack, unpack)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import qualified UI.HTML.Comp.StateButton as StateButton (html, ButtonType (..))
-- import qualified UI.Types.Elea as Elea

--import           Elea.Base (Computer, Story)
--import           Elea.Index (ComputerIndex)
--import           Elea.Set (SetKind)
--import qualified Elea.Set as Set (SetKind (..))
--import qualified Elea.Server as Server (Kind (..))


-- | View HTML
html :: Assets -> Html
html assets = return ()
  --H.div ! A.class_ "comp-set-builder" $ do
    --H.div ! classes [cls "content"] $ do
      ----H.div ! classes [cls "header", "comp-header"] $ do
        ----StateButton.html StateButton.Sidebar (eleaState setKind) assets
      --H.div ! classes [cls "set", cls setKindClass] $ do
        --defaultServerSetsHtml setKind
    --H.script $ 
         --"new Muuri('.comp-set-builder-set." <> H.toMarkup (cls setKindClass) <> "', {"
      -- <> "  dragEnabled: true,"
      --setKind  <> "});"
  -- where
    --eleaState Set.Servers   = Elea.CollectServers
    --eleaState Set.Stories   = Elea.CollectStories
    --eleaState Set.Computers = Elea.CollectComputers
    --stateClass st = T.unpack $ Elea.stateVerb st <> "-" <> Elea.stateObject st
    --setKindClass = stateClass $ eleaState setKind


--defaultServerSetsHtml :: Set.SetKind -> Html
--defaultServerSetsHtml Set.Servers = do
  --serverSetHTML Server.Internet
  --serverSetHTML Server.EleaClient
--defaultServerSetsHtml Set.Stories = storySetHTML 
--defaultServerSetsHtml Set.Computers = computerSetHTML


--serverSetHTML :: Server.Kind -> Html
--serverSetHTML serverKind = do
  --H.div ! A.class_ "set-server set" $ do
    --H.div ! A.class_ "set-server-content set-content" $ do
      --H.div ! A.class_ "set-server-kind set-kind" $ serverKindLabel serverKind
      --case serverKind of
        --Server.Internet   -> serverSetInternetHTML
        --Server.EleaClient -> serverSetEleaClientHTML
  --where
    --serverKindLabel Server.Internet   = "internet"
    --serverKindLabel Server.EleaClient = "elea app"


--serverSetInternetHTML :: Html
--serverSetInternetHTML = do
  --H.div ! A.class_ "set-server-internet" $ do
    --H.table $ do
      --H.tr $ do
        --H.th ! A.class_ "set-server-name-label" $ "Name"
        --H.td ! A.class_ "set-server-name-value" $ "EleaWorld" 
      --H.tr $ do
        --H.th ! A.class_ "set-server-uri-label" $ "URI"
        --H.td ! A.class_ "set-server-uri-value set-link" $ "world.elea.computer"


--serverSetEleaClientHTML :: Html
--serverSetEleaClientHTML = do
  --H.div ! A.class_ "set-server-elea-client" $ do
    --H.table $ do
      --H.tr $ do
        --H.th ! A.class_ "set-server-name-label" $ "Name"
        --H.td ! A.class_ "set-server-name-value" $ "Browser" 
      --H.tr $ do
        --H.th ! A.class_ "set-server-uri-label" $ "URI"
        --H.td ! A.class_ "set-server-uri-value set-link" $ "/computer/local-storage/story"


--storySetHTML :: Html
--storySetHTML = do
  --H.div ! A.class_ "set-story set" $ do
    --H.div ! A.class_ "set-story-content set-content" $ do
      --H.div ! A.class_ "set-story-kind set-kind" $ "name is like"
      --storySetNameHTML


--storySetNameHTML :: Html
--storySetNameHTML = do
  --H.div ! A.class_ "set-story-name" $ do
    --H.table $ do
      --H.tr $ do
        --H.th ! A.class_ "set-story-name-label" $ "Name"
        --H.td ! A.class_ "set-story-name-value" $ "any" 


--computerSetHTML :: Html
--computerSetHTML = do
  --H.div ! A.class_ "set-computer set" $ do
    --H.div ! A.class_ "set-computer-content set-content" $ do
      --H.div ! A.class_ "set-computer-kind set-kind" $ "abstraction"
      --computerSetAbsHTML


--computerSetAbsHTML :: Html
--computerSetAbsHTML = do
  --H.div ! A.class_ "set-computer-abstraction" $ do
    --H.table $ do
      --H.tr $ do
        --H.th ! A.class_ "set-computer-abs-label" $ "Name"
        --H.td ! A.class_ "set-computer-abs-value" $ "any" 
      --H.tr $ do
        --H.th ! A.class_ "set-computer-name-label" $ "Value"
        --H.td ! A.class_ "set-computer-name-value" $ "any" 



---- | HTML helper combinators 
--classes :: [String] -> H.Attribute
--classes = A.class_ . H.toValue . unwords

--cls :: String -> String
--cls s = "comp-set-builder-" <> s 
