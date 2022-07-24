--
-- HTML: Page
--

{-# LANGUAGE OverloadedStrings #-}

module Web.Handler where


import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant (Handler)
import Text.Blaze.Html (Html)

import Data.Assets (Assets (..))
import Web.HTML.Comp.View as View
import Web.HTML.Page (documentHTML)
import qualified Web.Types.View as View


page :: Assets -> Text -> Handler Html
page assets _ = return $ do
  documentHTML assets $ 
    View.html assets (View.new View.DefineArrow)
    


