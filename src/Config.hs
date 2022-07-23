--
-- Site Configuration
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

{-# LANGUAGE OverloadedStrings #-}

module Config where


import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))



data Config = Config {
    port     :: Int
  , iconsDir :: FilePath
}


instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config             <$>
    v .:   "port"      <*>
    v .:   "icons_dir"
  parseJSON _ = fail "Expected Object for Config value"



-- read config from file

newtype ConfigFromFileErr = ConfigFromFileErr Y.ParseException

configFromFile :: IO (Either ConfigFromFileErr Config)
configFromFile = do
  eConfig <- Y.decodeFileEither "./config.yaml"
  case eConfig of
    Left err -> return $ Left $ ConfigFromFileErr err
    Right conf -> return $ Right conf


