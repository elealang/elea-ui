--
-- Site Configuration
--

{-# LANGUAGE OverloadedStrings #-}

module UI.Config where


import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

--import Elea.Server (Server)


-- | Config
data Config = Config {
    server :: ServerConfig
--  , elea   :: EleaConfig
}

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config          <$>
    v .:   "server"
 --   v .:   "elea"
  parseJSON _ = fail "Expected Object for Config value"


-- | Web server configuration
data ServerConfig = ServerConfig {
    port     :: Int
  , iconsDir :: FilePath
}

instance FromJSON ServerConfig where
  parseJSON (Y.Object v) =
    ServerConfig       <$>
    v .:   "port"      <*>
    v .:   "icons_dir"
  parseJSON _ = fail "Expected Object for Config value"


-- | Elea config
--newtype EleaConfig = EleaConfig {
  --servers :: [Server] 
--}

--instance FromJSON EleaConfig where
  --parseJSON (Y.Object v) =
    --EleaConfig         <$>
    --v .:   "servers"
  --parseJSON _ = fail "Expected Object for Config value"


-- | Error parsing config
newtype ConfigFromFileErr = ConfigFromFileErr {
  getConfigFromFileErr :: Y.ParseException
}


-- | Parse the config from a file
configFromFile :: IO (Either ConfigFromFileErr Config)
configFromFile = do
  eConfig <- Y.decodeFileEither "./config.yaml"
  case eConfig of
    Left err -> return $ Left $ ConfigFromFileErr err
    Right conf -> return $ Right conf


