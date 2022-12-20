
module UI.Temp where


import qualified Elea
import Data.Foldable (foldl')
import Elea (name, description)

-- temporarily define hello world here

helloworldProgram :: Elea.Program
helloworldProgram = Elea.initProgram program' Elea.nowhere identity
  where
    identity = Elea.ProgramIdentity {
        id          = Elea.ProgramId "hello-world"
      , name        = Elea.ProgramName "Hello, World!"
      , description = Elea.ProgramDescription ""
    }
    program' = Elea.Program' {
      identity = identity    
    , space    = space  
    , time     = time  
    , agency   = agency
    }
    space = Elea.newSpace [absMain]


absMain :: Elea.Abstraction
absMain = 
  let stateRead = Elea.State {
          id          = Elea.StateId "read"
        , name        = Just $ Elea.StateName "Read"
        , description = Just $ Elea.StateDescription "The \"Hello, World!\" greeting text can be read"
        }
      stateReader = Elea.State {
          id          = Elea.StateId "reader"
        , name        = Just $ Elea.StateName "Reader"
        , description = Just $ Elea.StateDescription "The state of mind of the reader after reading \"Hello, World!\""
        }
      states = [
          stateRead
        , stateReader
        ]
      arrows = [
          Elea.Arrow {
            id             = Elea.ArrowId "read"
          , name           = Just $ Elea.ArrowName "Read"
          , description    = Just $ Elea.ArrowDescription "Read. Transition from the state of reading physical text to some mind state."
          , initStateIdRef = Elea.StateId "read"
          , termStateIdRef = Elea.StateId "reader"
          }
        ]
      initAbs = Elea.newAbstraction $ Elea.AbstractionId "main"
      _abs = foldl' Elea.abstractionWithState initAbs states
      abs' = foldl' Elea.abstractionWithArrow _abs arrows
  in  abs' {
        Elea.name        = Just $ Elea.AbstractionName "Main"
      , Elea.description = Just $ Elea.AbstractionDescription "Main abstraction for Hello, World! program"
      }


time :: Elea.Time
time =
  let initTime = Elea.newTime 
      computers = [
          computerWebsite
        , computerLanguageComprehension
        ]
      time' = foldl' Elea.timeWithComputer initTime computers
  in  time'


computerWebsite :: Elea.Computer
computerWebsite = 
  let ref       = Elea.nowhere
      computer_ = Elea.newComputer (Elea.ComputerId "website") ref
  in  computer_ {
        Elea.name        = Just $ Elea.ComputerName "hello-world.computer"
      , Elea.description = Just $ Elea.ComputerDescription "DNS accessible website for displaying the Hello, World! text"
      }

computerLanguageComprehension :: Elea.Computer
computerLanguageComprehension = 
  let ref       = Elea.nowhere
      computer_ = Elea.newComputer (Elea.ComputerId "english-language-comprehension") ref
  in  computer_ {
        Elea.name        = Just $ Elea.ComputerName "English Language Comprehension"
      , Elea.description = Just $ Elea.ComputerDescription "The human ability to visualize process symbolic form and react according to predetermined capabilities"
      }


agency :: Elea.Agency
agency = Elea.Agency {
    agents = [
        agentReader
      , agentServer
      ]
  , epic   = Elea.Epic Elea.existence []
}

agentReader :: Elea.Agent
agentReader = Elea.Agent {
    id          = Elea.AgentId "reader"
  , name        = Just $ Elea.AgentName "Reader"
  , description = Just $ Elea.AgentDescription "A person with the capability to read the text"
  , computers   = []
}


agentServer :: Elea.Agent
agentServer = Elea.Agent {
    id          = Elea.AgentId "server"
  , name        = Just $ Elea.AgentName "Server"
  , description = Just $ Elea.AgentDescription "The server which enables the website to function"
  , computers   = []
}
