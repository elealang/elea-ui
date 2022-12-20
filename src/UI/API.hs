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
  , Get, Raw, Capture, Header, QueryParam
  )
import Text.Blaze.Html (Html)


type API = 
      --  /static/{asset_type}/{asset_path}
      --    Example:
      --      /static/css/program/elea/program/view-basic.css
          "static" :> Raw
      --  redirect
    :<|>  Get '[HTML] Html
      --  /{program}/{state}?premise={storyId}
    :<|>  "program" :> "develop" :> "main" :> "home" 
            :> Get '[HTML] Html
    :<|>  "program" :> "elea" :> "program" :> "view-basic" 
            :> QueryParam "program-id" Text
            :> Get '[HTML] Html
    :<|>  "view" :> "elea" :> "program" :> "view-basic" :> "program-objects"
            :> QueryParam "program-id" Text 
            :> QueryParam "object-type" Text
            :> QueryParam "object-id" Text
            :> Get '[HTML] Html
    :<|>  "view" :> "elea" :> "program" :> "view-basic" :> "program-object"
            :> QueryParam "program-id" Text 
            :> QueryParam "object-type" Text
            :> QueryParam "object-id" Text
            :> Get '[HTML] Html
    



    -- :<|>  "program"
       -- :> Capture "program" Text 
       -- :> Capture "abstraction" Text 
       -- :> Capture "state" Text 
       -- :> QueryParams "premise" Text 
       -- :> Get '[HTML] Html
    -- :<|>  "view"
       -- :> Capture "program" Text 
       -- :> Capture "abstraction" Text 
       -- :> Capture "state" Text 
       -- :> Capture "view" Text 
       -- :> Get '[HTML] Html


--type APStringI = Get '[HTML] Html
      -- :<|> "create-story" :> Get '[HTML] Html
      -- :<|> "find-story" :> Get '[HTML] Html
      -- :<|> "static" :> Raw
