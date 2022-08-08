--
-- Web Computer: Local Storage
--


module UI.Elea.Computer.LocalStorage (
    putStory
  ) where


import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import           Data.Text (Text)
import qualified Data.Text as T (pack)

import           Elea.Base (Story)


putStory :: Story -> Text
putStory story =
  let storyJSON = T.pack $ LBS.unpack $ encode story 
  in  "localStorage.setItem(" <> story.id <> ", " <> storyJSON <> ");"
    
