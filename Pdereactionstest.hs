{-# LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable,TemplateHaskell #-}
-- +Require ModML-Units
-- +Require ModML-Reactions
-- +Require typehash
-- +Require template-haskell
-- +Require mtl
-- +Require syb
-- +Require containers
module ReactionModel
where
import qualified Pdeunits as U
import qualified Pdecore as B
import PDEUnitsOpAliases
import qualified Pdereactions as R
import qualified Data.Data as D
import qualified Data.TypeHash as D
import SIUnits


-- NOTE MAY WANT to add in a function to have a defualt diffusion co -efficient if there is non supplied


--in the ModMl reactions test example the way a reaction was made by

{-declaring constants variables and entities
  units are created,
  constants are created with units and U.realConstant
  entities are created using R.declareNamedTaggedEntity _ _ _     -}


{-using entities, declare rate equations
  in a do statement
   create equations using R.rateEquation                          -}


{-using rate equations declare processes 
  in this process the actual processes are built and the initial and boundary 
  conditions in a do statement

  R.newProcess
  R.addEntityInstance                                             -}


-- Testing pdereactions


--basic stuff
--declaring constants and units
pokeConstant = U.realConstant (uJoule) 2
pokeConc = uMole
pokeConcPerSec = uMole $*$ uSecond $**$ (-1)
pokeConcR = U.liftUnits pokeConc

--delcaring entities
R.declareNamedTaggedEntity [e|pokeConcR|] "pikachu" "pikachu"
R.declareNamedTaggedEntity [e|pokeConcR|] "thunderstone" "thunderstone"
R.declareNamedTaggedEntity [e|pokeConcR|] "raichu" "raichu"



--need to look into rate template and how that is done. also need to 
--add a template for building the partial diffusion terms as well as the
-- reaction term.

-- main1 = print model

-- --declaring processes
-- --1 pikachu and 1 thunderstone react to form a raichu
-- pika_evolve_React = do
--   pika <- R.addEntity (-1) pikachu
--   thunder <- R.addEntity (-1) thunderstone
--   rai <- R.addEntity 1 raichu
--   R.rateEquation $ (pika .+. thunder) .*. ( U.realConstant (uSecond $**$ (-1)) 1)


-- reactionModel = do
--   R.newProcess pika_evolve_React
  
--   --initial condition there is initially 1 pikachu
--   R.addEntityInstance pikachu (R.entityFromProcesses 
--                                (R.boundaryEquation 
--                                 (R.boundaryCondition (U.realConstant U.askTimeUnits 0) U.time) 
--                                  (U.realConstant pokeConc 1) (U.realConstant pokeConc 1)) 
--                                   (U.realConstant (pokeConc $*$ uSecond $**$ (-1)) 0))
  
--   --initial condition there is initially 1 thunderstone
--   R.addEntityInstance thunderstone (R.entityFromProcesses 
--                                     (R.boundaryEquation 
--                                      (R.boundaryCondition (U.realConstant U.askTimeUnits 0) U.time) 
--                                       (U.realConstant pokeConc 1) (U.realConstant pokeConc 1)) 
--                                        (U.realConstant (pokeConc $*$ uSecond $**$ (-1)) 0))
  
  
-- unitsModel :: Monad m => U.ModelBuilderT m ()
-- unitsModel = do
--   R.runReactionBuilderInUnitBuilder 1 reactionModel
    
-- model = B.buildModel $ U.partialUnitsToCore uSecond uMetre uMetre uMetre unitsModel



-- next step is to build a for example the laplacian.... in the reactions module...
-- the general form of an equation is du/dt = D*\/^2*u + R(u)
-- the ODE module current has the form du/dt = R (u) so i need to add in the D and \/ component



--part two.. now with the diffusion co-efficient included

main2 = print model

--declaring processes
--1 pikachu and 1 thunderstone react to form a raichu
pika_evolve_React = do
  pika <- R.addEntity (-1) pikachu
  thunder <- R.addEntity (-1) thunderstone
  rai <- R.addEntity 1 raichu
  R.rateEquation $ (pika .+. thunder) .*. ( U.realConstant (uSecond $**$ (-1)) 1)


reactionModel = do
  R.newProcess pika_evolve_React
  
  
  let lSqPerT = (U.askXUnits `U.unitsPowX` 2) `U.unitsTimesX` (U.askTimeUnits `U.unitsPowX` (-1)) 
  
--initial condition there is initially 1 pikachu
  R.addEntityInstance pikachu (R.entityFromProcesses 
                               (R.boundaryEquation 
                                (R.boundaryCondition (U.realConstant U.askTimeUnits 0) U.time) 
                                 (U.realConstant pokeConc 1) (U.realConstant pokeConc 1)) 
                               
                                  (U.realConstant (pokeConc $*$ uSecond $**$ (-1)) 0)
                               
                                   --(R.mkScalarDiffCoeff (U.realConstant lSqPerT 3))
                                   (R.mkScalarDiffCoeff (U.realConstant lSqPerT 3))
                              )
  
  --initial condition there is initially 1 thunderstone
  R.addEntityInstance thunderstone (R.entityFromProcesses 
                                    (R.boundaryEquation 
                                     (R.boundaryCondition (U.realConstant U.askTimeUnits 0) U.time) 
                                      (U.realConstant pokeConc 1) (U.realConstant pokeConc 1)) 
                                       (U.realConstant (pokeConc $*$ uSecond $**$ (-1)) 0)
                                        (R.mkScalarDiffCoeff (U.realConstant lSqPerT 2))
                                   )
  
  
unitsModel :: Monad m => U.ModelBuilderT m ()
unitsModel = do
  R.runReactionBuilderInUnitBuilder 1 reactionModel
    
model = B.buildModel $ U.partialUnitsToCore uSecond uMetre uMetre uMetre unitsModel