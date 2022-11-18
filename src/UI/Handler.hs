--
-- HTML: Page
-- Just two routes?

module UI.Handler where


import           Data.Text (Text)
import           Servant (
    Handler
  , ServerError (errHeaders), throwError, err301
  )
import           Text.Blaze.Html (Html)

import           UI.Data.Assets (Assets (..))
import qualified UI.HTML.Page as Page
import qualified UI.Types.Elea.Program.Develop as Program
import qualified UI.Types.Page as Page

import qualified Elea.Constant.Program as Program (elea)
import qualified Elea.Atom.Phenomena as EleaAtom (
    AbstractionId (..)
  , State (..)
  )
import qualified Elea.Object.Program as EleaObject (
    ProgramState (..)
  )

-- | /
--
-- The home page. Since everything is a program, it redirects to the
-- Develop program route automatically.
pageHome :: Handler Html
pageHome = throwError err301 { 
    errHeaders = [("Location", "/develop/main/home")] 
  }


-- | /{program}/{abstraction}/{program_state}/?effect=storyX&effect=storyY
--
-- Could be generalized to the below routes, except that all of the abstractions
-- are of type program
-- /{abstraction}/{state}/?effects
pageProgram :: Assets -> Text -> Text -> Text -> [Text] -> Handler Html
pageProgram assets programId absId _state effects = do
  -- what this will do ->
  -- look up program by program id
  -- will go to a server (internet computer) that computes the program
  -- for now we will mock it
  -- get state of program
  -- convert state to html
  --   such that HTML is container for arrows in state
  --     and links in html leader to other states/renderings
  -- for now we mock it -> 
  case programId of
    "develop" -> do
      let programState  = EleaObject.ProgramState {
          abstractionId = EleaAtom.AbstractionId "develop" 
        , state         = EleaAtom.State _state
        }
      return $ Page.html "" $ 
        Page.new assets Program.develop programState

    "elea"    -> do
      let programState  = EleaObject.ProgramState {
          abstractionId = EleaAtom.AbstractionId absId 
        , state         = EleaAtom.State _state
        }
      return $ Page.html "" $ 
        Page.new assets Program.elea programState
    _         -> return $ return ()

