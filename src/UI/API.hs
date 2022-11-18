--
-- API
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module UI.API (
    API
  ) where


import Data.Text (Text)
import Servant.HTML.Blaze (HTML)
import Servant.API ( 
    (:>), (:<|>)
  , Get, Raw, Capture, Header, QueryParams
  )
import Text.Blaze.Html (Html)


type API = 
          "static" :> Raw
    -- / (redirects)
     :<|> Get '[HTML] Html
    -- /{program}/{abstraction}/{state}?effect={storyId}
    -- :<|>  Capture "program" Text 
       -- :> Capture "abstraction" Text 
       -- :> Capture "state" Text 
       -- :> Get '[HTML] Html
    -- /static/{asset_type}/{asset}
    -- /{program}/{state}?effect={storyId}
    :<|>  Capture "program" Text 
       :> Capture "abstraction" Text 
       :> Capture "state" Text 
       :> QueryParams "effect" Text 
       :> Get '[HTML] Html


--type APStringI = Get '[HTML] Html
      -- :<|> "create-story" :> Get '[HTML] Html
      -- :<|> "find-story" :> Get '[HTML] Html
      -- :<|> "static" :> Raw
