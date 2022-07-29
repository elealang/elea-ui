--
-- HTML: Page
--

{-# LANGUAGE OverloadedStrings #-}

module Web.Handler where


import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text.IO as T (putStrLn)
import           Servant (Handler)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           Data.Assets (Assets (..))
import           Config (EleaConfig (..))
import qualified Web.HTML.Page.Home as PageHome (html)
import qualified Web.HTML.Page.Build as PageBuild (html)
import qualified Web.Types.State as State
import qualified Web.Types.View as View


pageHome :: Assets -> Handler Html
pageHome assets = return $
  PageHome.html assets 


pageMainCreateStory :: EleaConfig -> Assets -> Handler Html
pageMainCreateStory config assets = return $ 
  PageBuild.html config assets State.MainCreateStory

 -- where
    --state       = State.MainCreateStory
    --headerHTML  = ViewSwitcher.html (View.new state) assets
    --contentHTML = Home.html assets
    --doc         = Document assets headerHTML contentHTML False

