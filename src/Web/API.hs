--
-- API
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Web.API (
    API
  ) where


import Data.Text (Text)
import Servant.HTML.Blaze (HTML)
import Servant.API
  ( (:>), (:<|>)
  , Get, Raw, Capture, Header
  )
import Text.Blaze.Html (Html)


type API = Get '[HTML] Html
      :<|> "create-story" :> Get '[HTML] Html
      :<|> "static" :> Raw


