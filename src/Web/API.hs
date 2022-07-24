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
  , Get, Raw, Capture
  )
import Text.Blaze.Html (Html)


type API = "s" :> Capture "state" Text :> Get '[HTML] Html
      :<|> "static" :> Raw

