--
-- HTML: Page
-- Just two routes?

module UI.Handler where


import qualified Data.ByteString.Lazy.UTF8 as BLU (fromString)
import qualified Data.HashMap.Strict as HM (elems)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           Servant (
    Handler
  , ServerError (errHeaders, errBody), throwError, err301, err400, err404
  )
import           Text.Blaze.Html (Html)

import           UI.Data.Assets (Assets (..))
import qualified UI.HTML.Program.Page as Program.Page
import qualified UI.HTML.Program.Develop.Home as Develop.Home (html)
import qualified UI.HTML.Program.Elea.ViewBasic as Elea.Program.ViewBasic (
    html
  , objectsHTML
  , objectHTML
  )
import qualified UI.Types.Page as Page
import qualified UI.Types.Elea.Program.Develop as Program
import qualified UI.Temp as Temp

import qualified Elea.Constant.Program as Program (elea)
import qualified Elea
import UI.HTML.Program.Elea.ViewBasic (objectsHTML)



--------------------------------------------------------------------------------
-- | PAGE | Programs
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | PAGE | Programs > Develop
--------------------------------------------------------------------------------

-- | /
-- / redirects to /program/develop/main/home 
root :: Handler Html
root = throwError err301 { 
    errHeaders = [("Location", "/program/develop/main/home")] 
  }

-- | /program/develop/main/home
programDevelopMainHome :: Assets -> Handler Html
programDevelopMainHome assets = do
  let page      = Page.new assets Program.develop
      stateHtml = Develop.Home.html page
  return $ Program.Page.html "" page stateHtml

--------------------------------------------------------------------------------
-- | PAGE | Programs > Elea
--------------------------------------------------------------------------------

programEleaProgramViewBasic :: Assets -> Maybe Text -> Handler Html
programEleaProgramViewBasic assets mProgramId = do
  let page      = Page.new assets Program.elea
      stateHtml = Elea.Program.ViewBasic.html page Temp.helloworldProgram
  return $ Program.Page.html "" page stateHtml


--------------------------------------------------------------------------------
-- | VIEWS | Programs -> Elea
--------------------------------------------------------------------------------

handleProgramObjects :: Maybe Text -> Maybe Text -> (Elea.ProgramId -> Elea.ObjectType -> Handler Html) -> Handler Html 
handleProgramObjects mProgramId mObjectType onSuccess = do
  case (mProgramId, mObjectType) of
    (Nothing       , Nothing      ) -> throwError $ err404 { 
      errBody = "Program ID required and Object ID required" 
    }
    (Nothing       , Just _       ) -> throwError $ err404 { 
      errBody = "Program ID required" 
    }
    (Just _        , Nothing      ) -> throwError $ err404 { 
      errBody = "Object Type required" 
      }
    (Just programId, Just objectType) -> 
      case Elea.objectTypeFromText objectType of
        Just object   -> onSuccess (Elea.ProgramId programId) object
        Nothing       -> throwError $ err400 { 
          errBody = "Unknown object type: " <> (BLU.fromString $ T.unpack objectType)
        }

-- | State views
viewEleaProgramViewBasicProgramObjects :: Assets -> Maybe Text -> Maybe Text -> Maybe Text -> Handler Html
viewEleaProgramViewBasicProgramObjects assets mProgramId mObjectType mObjectId = do
  handleProgramObjects mProgramId mObjectType onSuccess
    where
    onSuccess _ objectType = do
      let page = Page.new assets Program.elea
          program = Temp.helloworldProgram
          program' = head $ HM.elems program.programById
          objectRef = Elea.objectRefFromText objectType <$> mObjectId
      return $ Elea.Program.ViewBasic.objectsHTML page program' objectType objectRef

-- | Right-most pane html
viewEleaProgramViewBasicProgramObject :: Assets -> Maybe Text -> Maybe Text -> Maybe Text -> Handler Html
viewEleaProgramViewBasicProgramObject assets mProgramId mObjectType mObjectId = do
  handleProgramObjects mProgramId mObjectType onSuccess
    where
    onSuccess _ objectType = do
      case mObjectId of
        Just objectId -> do
          let page = Page.new assets Program.elea
              program = Temp.helloworldProgram
              program' = head $ HM.elems program.programById
              objectRef = Elea.objectRefFromText objectType objectId
          return $ Elea.Program.ViewBasic.objectHTML page program' objectRef
        Nothing       ->  throwError $ err404 { 
            errBody = "Object ID required" 
          }




-- notes
-- look up program by program id
-- will go to a server (internet computer) that computes the program
-- convert state to html
--   such that HTML is container for arrows in state
--     and links in html leader to other states/renderings
