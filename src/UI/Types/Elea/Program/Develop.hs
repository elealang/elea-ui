--
-- WEB: Types.EleaState
-- collection -- import as Program

module UI.Types.Elea.Program.Develop (
    develop
  ) where


import           Data.Foldable (foldl')

import qualified Elea


-- | Develop program
develop :: Elea.Program
develop = Elea.initProgram program' initState Elea.noIdentity
  where
    program' = Elea.Program' {
        identity = devIdentity
      , space    = devSpace
      , time     = devTime
      , agency   = devAgency
    }
    initState = Elea.Reference {
      programId     = Elea.ProgramId "develop" 
    , abstractionId = Elea.AbstractionId "main" 
    , stateId       = Elea.StateId "home"
    , arrowId       = Nothing
    }


-- | Identity
--------------------------------------------------------------------------------

devIdentity :: Elea.ProgramIdentity
devIdentity = Elea.ProgramIdentity {
   id          = Elea.ProgramId "develop"
 , name        = Elea.ProgramName "Develop"
 , description = Elea.ProgramDescription "Develop programs"
}

-- | Space
--------------------------------------------------------------------------------

devSpace :: Elea.Space
devSpace = Elea.newSpace [absView]


absView :: Elea.Abstraction
absView =
  let states = [
          Elea.State {
            id          = Elea.StateId "home"
          , name        = Just $ Elea.StateName "Home"
          , description = Just $ Elea.StateDescription ""
          }
        ]
      initAbs = Elea.newAbstraction $ Elea.AbstractionId "main"
  in  foldl' Elea.abstractionWithState initAbs states


    --let stateView = Obj.State {
            --id          = Def.StateId "view"
          --, name        = Just $ Obj.StateName "View"
          --, description = Just $ Obj.StateDescription ""
          --}
        --states = [
            --stateView
          --]
        --initAbs = Obj.newAbstraction $ Def.AbstractionId "program"
        --abs = foldl' Obj.abstractionWithState initAbs states
    --in  abs {
            --Obj.name        = Just $ Def.AbstractionName "Program"
          --, Obj.description = Nothing
        --}

-- | Time
--------------------------------------------------------------------------------

devTime :: Elea.Time
devTime = Elea.newTime

-- | Agency
--------------------------------------------------------------------------------
    
devAgency :: Elea.Agency
devAgency = Elea.Agency {
    agents = []
  , epic   = Elea.Epic Elea.existence []
}


-- eventually elea.dev could be a generic computer based 
-- on these programs

-- should generically implement program switching capability
-- so of course you can run your own programs with same engine
-- so could actually later use to bootstrap itself?
