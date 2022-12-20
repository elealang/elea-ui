--
-- HTML.Comp.ComputerChooser
--

{-# LANGUAGE OverloadedStrings #-}

module UI.HTML.Comp.ProgramStory (
    html
  ) where


import           Control.Monad (forM_, mapM_)
import           Data.Functor ((<&>))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import           Text.Blaze.Html5.Attributes as A

import           UI.Data.Assets (Assets (..))
import           UI.Types.Page (Page)

import qualified Elea


import Debug.Trace (traceShow)


-- | Story History HTML 
html :: Page -> Html
html page = do
  H.div ! A.class_ "comp-program-story" $ do
    H.div ! classes [cls "history"] $ do
      mapM_ stateHTML statePairs 
  where
   -- A list of tuples of (AbstractionName, StateName)
    program = page.program
    historicalStates = Elea.currentProgramHistoryStates page.program
    statePairs = historicalStates <&> (\ref -> 
        ( fromMaybe "" $ Elea.programAbstractionName program ref
        , fromMaybe "" $ Elea.programStateName program ref
        )
      )


stateHTML :: (Text, Text) -> Html
stateHTML (absName, stateName) = 
  H.div ! classes [cls "state"] $ do
    H.div ! classes [cls "state-abstraction"] $
      H.toMarkup absName
    H.div ! classes [cls "state-state"] $
      H.toMarkup stateName


-- | HTML helper combinators 
classes :: [String] -> H.Attribute
classes l = A.class_ $ H.toValue $ unwords l

cls :: String -> String
cls s = "comp-program-story-" <> s 
