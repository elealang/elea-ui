--
-- WEB: Types.EleaState
-- collection -- import as Program

module UI.Types.Elea.Program.Develop (
    develop
  ) where



import qualified Elea.Atom.Phenomena as Atm (
    AbstractionId (..)
  , State (..)
  )
import qualified Elea.Atom.Program as Atm (
    ProgramIdentity (..)
  , ProgramAlias (..), ProgramName (..), ProgramDescription (..)
  )
import qualified Elea.Object.Phenomena as Obj (
    Abstraction, newAbstraction
  , State (..), StateName (..), StateDescription (..)
  )
import qualified Elea.Object.Program as Obj (
    Program (..)
  , Space (..), newSpace
  , Time (..)
  , Agency (..)
  )


-- | Develop program
develop :: Obj.Program
develop = Obj.Program {
    id       = devIdentity
  , space    = devSpace
  , time     = devTime
  , agency   = devAgency
}

-- | Identity
--------------------------------------------------------------------------------

devIdentity :: Atm.ProgramIdentity
devIdentity = Atm.ProgramIdentity {
   alias       = Atm.ProgramAlias "develop"
 , name        = Atm.ProgramName "Develop"
 , description = Atm.ProgramDescription "Develop programs"
}

-- | Space
--------------------------------------------------------------------------------

devSpace :: Obj.Space
devSpace = Obj.newSpace [absView]


absView :: Obj.Abstraction
absView = Obj.newAbstraction _id _states
  where
    _id     = Atm.AbstractionId "main"
    _states = [
        Obj.State {
          id          = Atm.State "home"
        , name        = Obj.StateName "Home"
        , description = Obj.StateDescription ""
        }
      ]

-- | Time
--------------------------------------------------------------------------------

devTime :: Obj.Time
devTime = Obj.Time {
    types     = []
  , computers = []
}

-- | Agency
--------------------------------------------------------------------------------
    
devAgency :: Obj.Agency
devAgency = Obj.Agency {
    agents  = []
  , stories = []
  , events  = []
}




-- eventually elea.dev could be a generic computer based 
-- on these programs

-- should generically implement program switching capability
-- so of course you can run your own programs with same engine
-- so could actually later use to bootstrap itself?
