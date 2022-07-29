--
-- Site Configuration
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

{-# LANGUAGE OverloadedStrings #-}

module Config where


import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

import Elea.Base (Computer)


-- | Config
data Config = Config {
    server :: ServerConfig
  , elea   :: EleaConfig
}

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config          <$>
    v .:   "server" <*>
    v .:   "elea"
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
data EleaConfig = EleaConfig {
  computers :: [Computer] 
}

instance FromJSON EleaConfig where
  parseJSON (Y.Object v) =
    EleaConfig         <$>
    v .:   "computers"
  parseJSON _ = fail "Expected Object for Config value"


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


