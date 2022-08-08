--
-- HTML: Page
--

module UI.Handler where


import           Servant (Handler)
import           Text.Blaze.Html (Html)

import           UI.Data.Assets (Assets (..))
import           UI.Config (EleaConfig (..))
import qualified UI.HTML.Page.Elea as PageElea (html)
import           UI.Types.Elea as Elea (EleaState (..))
import           UI.Types.Page (PageElea (..))


pageHome :: EleaConfig -> Assets -> Handler Html
pageHome _config assets = return $
  PageElea.html (PageElea Elea.GeneralHome _config False) assets 


pageCreateStory :: EleaConfig -> Assets -> Handler Html
pageCreateStory _config assets = return $ 
  PageElea.html (PageElea Elea.EleaCreateStory _config True) assets 


pageFindStory :: EleaConfig -> Assets -> Handler Html
pageFindStory _config assets = return $ 
  PageElea.html (PageElea Elea.IndexFindStory _config True) assets 
