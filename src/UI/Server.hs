--
-- WEB / Server
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module UI.Server (
    runServer
  ) where


import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant 
  ( Server
  , serve
  , Proxy (..)
  , (:<|>)(..)
  , serveDirectoryWebApp
  )

import UI.Data.Assets (Assets)
import UI.Config (Config)
import UI.API (API)
import qualified UI.Handler as Handler


api :: Proxy API
api = Proxy


server :: Config -> Assets -> Server API
server config assets =  
        serveDirectoryWebApp "resources/"
   :<|> Handler.pageHome
   :<|> Handler.pageProgram assets


app :: Config -> Assets -> Application
app config assets = serve api $ server config assets


runServer :: Config -> Assets -> IO ()
runServer config assets = do
  run config.server.port $ app config assets

