{-# LANGUAGE DeriveDataTypeable,NoMonomorphismRestriction,TemplateHaskell #-}

--it is essential to include TemplateHaskell in order to create new base Units!!!

module Pdeunistest
where       
import qualified Pdeunits as U  
import qualified Pdecore as B 
import SIUnits
import PDEUnitsOpAliases
import qualified Data.Typeable as D
import qualified Data.TypeHash as D
import qualified Data.Data as D
import qualified Control.Monad as M
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I

--TO DO

--build up equations using units module                                                                                    DONE
--build equations using both core and units module                                                                         DONE    
--build my own units                                                                                                       DONE
--build up partial differential equation for a reaction diffusion problem similar to the CoreTest except with units        DONE
--possibly build more complicated equations including boundary equations etc..


--building the unit poke, with global access
U.declareBaseType "pokemon" "pokemonBase"  
poke = M.liftM U.singletonUnit pokemonBase


main1 = print testmodel


-- units for t x y and z can easily be declared because they are in the SI module

--need to figure out functions that build equations, expressions including units

-- I think that building new variables with the units is done by the function mkNamedrealVariable
unitsmodel = 
  do 
  a <- U.mkNamedRealVariable uMetre "charmander"  
  b <- U.mkNamedRealVariable uMetre "fire type"
  
  a `U.newEq` b
    
testmodel = B.buildModel (U.partialUnitsToCore uSecond uMetre uMetre uMetre unitsmodel) 

--building own units

--declare base units using the function declareBaseType <name> <VARname>
--once a base unit has been declared actually units can be made by using   " M.liftM U.singletonUnit <VARname> " to create a singletonUnit (has a multiplier of 1 and exponent of 1
--Units can be combined by using $*$ and raised to exponent by $**$
--Prefixes are available eg "uMicro" in the form of functions; which act as multipliers, eg uMicro would multiply by 10^-. This would be used with a unitsTimes or a $*$


--second example uses "poke" a unit that I have created
main2 = print testmodel2
  

makeunitsmodel = 
  do
  
  c <- U.mkNamedRealVariable poke "Pikachu" 
  d <- U.mkNamedRealVariable poke "Diglett"
  
  c `U.newEq` d
  

testmodel2 = B.buildModel (U.partialUnitsToCore uSecond uMetre uMetre uMetre makeunitsmodel)   


-- now to start a model off in the core and then fuse it with a model in the units module
-- This combines makunitsmodel (units module) and coremodel (core module)

main3 = print testmodelfuse

coremodel = B.buildModel $ do
  
  
  x <- B.newNamedRealVariable "Bulbasaur"
  y <- B.newNamedRealVariable "Squirtle"  
  
  x `B.newEq` y   
  

--building a function that will run a model in the core module, then this state value can be taken and built upon by the units module
  
runModelT :: Monad m => B.ModelBuilderT m a -> m B.BasicPDEModel
runModelT x = S.execStateT (B.modelBuilderTToState x) coremodel
runModel :: B.ModelBuilder a -> B.BasicPDEModel
runModel = I.runIdentity . runModelT


testmodelfuse = runModel (U.partialUnitsToCore uSecond uMetre uMetre uMetre makeunitsmodel)   





--Build a coupled partial differential equation similar to that used in the core module                         DONE
-- the reaction model is taken from http://mrob.com/pub/comp/xmorphia/
main4 = print testreactionmodel

--Global unit construction
uMicroMetre = uMicro $*$ uMetre
uPerMicroMetre3 = uMicroMetre $**$ (-3)
uM = uMole $*$ uPerMicroMetre3      
udiff = ( uMicroMetre $**$ 2) $*$ uSecond $**$ (-1)
uH = (uMole $**$ (-2)) $*$ (uMicroMetre $**$ (6)) $*$ (uSecond $**$ (-1))

reactionmodel = do
  
  --building variables
  u <- U.mkNamedRealVariable uM "u"
  v <- U.mkNamedRealVariable uM "v"
  diffU <- U.mkNamedRealVariable udiff "Diffusion constant for u"
  f <-U.mkNamedRealVariable (uSecond $**$ (-1)) "Source/Sink term"
  diffV <-U.mkNamedRealVariable udiff "Diffusion constant for v"
  k <- U.mkNamedRealVariable U.dimensionless "Rate at which V-> P"
  h <- U.mkNamedRealVariable uH "constant to multiply uv^2 "
  
  
  --build laplacian and constants
  let laplacianU = (U.partialX (U.partialX u)) .+. (U.partialY (U.partialY u)) .+. (U.partialZ (U.partialZ u)) 
  let laplacianV = (U.partialX (U.partialX v)) .+. (U.partialY (U.partialY v)) .+. (U.partialZ (U.partialZ v))
  let dimensionless2 = U.realConstant U.dimensionless 2
  let dimensionless1 = U.realConstant U.dimensionless 1    
  let um1 = U.realConstant uM 1    
              
  --equations          
  U.partialTime u `U.newEq` ( (laplacianU .*. diffU)  .-.  (h .*. u .*. v .**. dimensionless2)  .+.  (f .*. ( um1 .-. u )) )
  U.partialTime v `U.newEq` ( (laplacianV .*. diffV)  .+.  (h .*. u .*. v .**. dimensionless2)  .-.  (f .*. v .*. ( dimensionless1 .+. k)))
                             
                             
testreactionmodel = B.buildModel (U.partialUnitsToCore uSecond uMicroMetre uMicroMetre uMicroMetre reactionmodel)




--simple model using laplacian
main5 = print testsimplemodel

simplemodel = do
  
  u <- U.mkNamedRealVariable uM "u"
  v <- U.mkNamedRealVariable uM "v"
  
  --laplacian also works
  let laplacianU = (U.partialX (U.partialX u)) .+. (U.partialY (U.partialY u)) .+. (U.partialZ (U.partialZ u)) 
  let laplacianV = (U.partialX (U.partialX v)) .+. (U.partialY (U.partialY v)) .+. (U.partialZ (U.partialZ v))
  
  
  (laplacianU .*. u) `U.newEq` (laplacianV .*. v)
  
testsimplemodel = B.buildModel (U.partialUnitsToCore uSecond uMicroMetre uMicroMetre uMicroMetre simplemodel)
  
  
  