--
-- API
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Web.API (
    API
  ) where


import Servant.HTML.Blaze (HTML)
import Servant.API
  ( (:>), (:<|>)
  , Get, Raw
  )

import Text.Blaze.Html (Html)


type API = Get '[HTML] Html
      :<|> "static" :> Raw

