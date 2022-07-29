--  
-- Main 
--  

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Yaml as Y

import Config (Config (..), ConfigFromFileErr (..), configFromFile)
import Data.Assets (Assets (..))
import Data.Icon (IconIndex)
import qualified Data.Icon as I
  ( iconSVGWithName
  , load
  )
import Web.Server as Web (runServer)


main :: IO ()
main = do
  putStrLn "Reading configuration: config.yaml"
  eConf <- configFromFile
  case eConf of
    Left (ConfigFromFileErr err) -> do
      print $ Y.prettyPrintParseException err
    Right conf -> do
      putStrLn "Loading icons: assets/icons/"
      mIconIndex <- I.load conf.server.iconsDir
      putStrLn $ "Starting web server: PORT=" ++ show conf.server.port
      case mIconIndex of
        Just iconIndex -> do
          Web.runServer conf $ Assets iconIndex
        _ -> return ()

