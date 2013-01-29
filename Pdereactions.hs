{-# LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts,FlexibleInstances,TemplateHaskell #-}

{- |
The ModML reactions module used to describe a system in terms of processes on
entities.
-}
module Pdereactions
where

import qualified Data.Set as S
import Data.Set ((\\))
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Data as D
import qualified Data.TypeHash as D
import qualified Pdeunits as U
import qualified Pdecore as B
import qualified Pdecore as C
import qualified SIUnits as SI
import Control.Monad
import qualified Control.Monad.Trans as M
import qualified Control.Monad.Reader as R
import Data.Maybe
import Data.Generics
import Data.List hiding ((\\))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import qualified Language.Haskell.TH.Syntax as T
import qualified Data.Char as C
import PDEUnitsOpAliases
import qualified Debug.Trace as DT


--Entity represents a thing such as a reactive species or a more abstract species such as energy or heat
data Entity = Entity U.Units Int deriving (Eq, Ord, Show, D.Typeable, D.Data)

--Amount represents how much of an entity there
data Amount = Amount Double U.Units deriving (Show)

-- NEW ***
--should be enough to cover both initial and boundary conditions, initial conditions simply have a at t=0, which can easily be incorperated
type Condition = U.BoolExpression
type LeftHandSide = U.RealExpression
type RightHandSide = U.RealExpression
--data structure for building the diffusion tensor, X Y and Z represent the rows and every element is a RealExpression
--type X = [U.RealExpression, U.RealExpression, U.RealExpression]
--type Y = [U.RealExpression, U.RealExpression, U.RealExpression]
--type Z = [U.RealExpression, U.RealExpression, U.RealExpression]
  
type Re = U.RealExpression

data Partial = Partial U.RealExpression U.RealExpression U.RealExpression    

data Boundary = Boundary Condition LeftHandSide RightHandSide deriving (D.Typeable, D.Data, Show)

-- the actual form of the diffusion term is actually \/. (D*\/*u)
-- the Diffusion co-efficient therefore can depend on all spatial terms....
-- the diffusion co-efficient is a tensor field. so at every point on space there is a 3x3 tensor
data Diffusion = Diffusion Re Re Re Re Re Re Re Re Re deriving (D.Typeable, D.Data, Show)

mapDiffusion f (Diffusion d1 d2 d3 d4 d5 d6 d7 d8 d9) = Diffusion (f d1) (f d2) (f d3) (f d4) (f d5) (f d6) (f d7) (f d8) (f d9)

                        
data BoundaryList = BoundaryList [Boundary]


--Process describes the transformation of amount of entities. It describes what CAN happen. In this new version however it will usually describe what WILL happen.
data Process = Process {
      
      -- The unique identifier for the Process.
      processId :: Int,
      
     
      {- | A mapping from placeholder variables to the Entity
      amount they represent. This is required because there is no U.ID supplied to entities until the processed 
      through U.modelbuilder
      -}
      entityVariables :: M.Map U.RealVariable Entity,
      
      {- | The stoichiometry of each CompartmentEntity; this consists of a
      multiplier to be applied for each CompartmentEntity when computing
      the flux, and the units of that multiplier. It should be negative for
      reactants, and positive for products.
      -}
      stoichiometry :: M.Map Entity (Double, U.Units),
      
      {- | A template for the process rate. Fluxes are generated from this as follows:
      * Placeholders in entityVariables will be replaced.
      * The multiplier specified in the stoichiometry is applied. 
      -}
      rateTemplate :: U.RealExpression
      
    } deriving (D.Typeable, D.Data)
               
               
instance Eq Process where
    Process { processId = ida} == Process { processId = idb } = ida == idb
instance Ord Process where
    Process { processId = ida} `compare` Process { processId = idb } = ida `compare` idb
    
    

data EntityInstance =
    {- | The entity amount is known, but the reactions occurring don't
    change it. This can be used for several purposes:
 
    * The amount can be fixed if changes over time are so negligible that
    it is simpler to fix the level.
    * The value being clamped to could be a variable from a different part
    of the model.
    -}
    EntityClamped U.RealExpression |
    
    
    {- | The entity amount starts at an initial value, and changes with time as
    a result of the processes acting on it. The second field stores the expression
    to be added to the fluxes to make the final rate of change.
    -}
    
    --added 3 dimentional initial and Boundary conditions to EntityFromProcesses
    --first constructor is an initial condition, second is a boundary condition,
    -- last is the flux
    EntityFromProcesses Boundary U.RealExpression Diffusion
    
     -- The diffusion co-efficient, belongs to each entity
      
    deriving (D.Typeable, D.Data)

    
--more support for EntityInstance... eg boundary conditions for space as well as temporal conditions


    
data ReactionModel = ReactionModel {
 
      -- Functions which should be used to generate all Processes
      allProcesses :: [Process],
      
      
      {- | A map from each entity to the description of the
      entity which contains it.
      -}
      entityInstances :: M.Map Entity EntityInstance,

      -- Annotations on the reaction model...
      annotations :: M.Map (String, String) String,

      -- Context tagged IDs, tagged to a metatype and a tag type.
      contextTaggedIDs :: M.Map (D.TypeCode, D.TypeCode) Int,

      -- The next identifier to allocate.
      nextID :: Int
      
    } deriving (D.Typeable, D.Data)
               

entityClamped :: Monad m => U.ModelBuilderT m U.RealExpression -> ModelBuilderT m EntityInstance
entityClamped ex = do
  ex' <- U.liftUnits ex
  return $ EntityClamped ex'

--added Boundary values
entityFromProcesses :: Monad m => U.ModelBuilderT m Boundary -> U.ModelBuilderT m U.RealExpression -> U.ModelBuilderT m Diffusion ->  ModelBuilderT m EntityInstance
entityFromProcesses bv flux diffusion = do
  bv' <- U.liftUnits bv
  flux' <- U.liftUnits flux
  diffusion' <- U.liftUnits diffusion
  return $ EntityFromProcesses bv' flux' diffusion'
               
-- all I need to do is to keep building processes to the reaction model               
emptyReactionModel = ReactionModel {
                       
                       allProcesses = [],
                       entityInstances = M.empty,
                       annotations = M.empty,
                       contextTaggedIDs = M.empty,
                       nextID = 0
                     }

-- The independent variable units for time
independentTimeUnits = SI.uSecond

-- The independent variable units for space
independentSpaceUnits = SI.uMetre

-- NEW functions for building the laplacian depending on the diffusion Co-efficient
laplacian1D u = (U.PartialSpaceX (U.PartialSpaceX u)) 
laplacian2D u = (U.PartialSpaceX (U.PartialSpaceX u)) `U.Plus` (U.PartialSpaceY (U.PartialSpaceY u)) 
laplacian3D u = (U.PartialSpaceX (U.PartialSpaceX u)) `U.Plus` (U.PartialSpaceY (U.PartialSpaceY u)) `U.Plus` (U.PartialSpaceZ (U.PartialSpaceZ u))
                
diffusionDlaplacian :: U.RealExpression -> Int -> U.RealExpression    
diffusionDlaplacian u 1 = laplacian1D u 
diffusionDlaplacian u 2 = laplacian2D u
diffusionDlaplacian u 3 = laplacian3D u
diffusionDlaplacian u e = error $ "invalid dimension of diffusion. You entered: " ++ (show e) ++ ". Please choose 1, 2 or 3" 




--NEW need to build gradient operator

grad u = Partial (U.PartialSpaceX u) (U.PartialSpaceY u) (U.PartialSpaceZ u)

-- Need to incorperate the diffusion co-efficient
-- diffusion constant in 3 dimensions x y and z... the units should be length^2 * time ^-1
-- function for creating the diffusion matrix


zero = U.realConstantE U.dimensionlessE 0


mk3x3DiffCoeffM :: Monad m => Re -> Re -> Re -> Re -> Re -> Re -> Re -> Re -> Re -> U.ModelBuilderT m Diffusion
mk3x3DiffCoeffM x1 x2 x3 y1 y2 y3 z1 z2 z3 = return $ Diffusion x1 x2 x3 y1 y2 y3 z1 z2 z3
mk3x3DiffCoeff :: Monad m => U.ModelBuilderT m Re -> U.ModelBuilderT m Re -> U.ModelBuilderT m Re -> U.ModelBuilderT m Re -> U.ModelBuilderT m Re -> U.ModelBuilderT m Re -> U.ModelBuilderT m Re -> U.ModelBuilderT m Re -> U.ModelBuilderT m Re -> U.ModelBuilderT m Diffusion
mk3x3DiffCoeff x1 x2 x3 y1 y2 y3 z1 z2 z3 = do
  x1' <- x1
  x2' <- x2
  x3' <- x3
  y1' <- y1
  y2' <- y2
  y3' <- y3
  z1' <- z1
  z2' <- z2
  z3' <- z3
  mk3x3DiffCoeffM x1' x2' x3' y1' y2' y3' z1' z2' z3'   


--this is kinda pointless
mk3x1DiffCoeffM x y z = mk3x3DiffCoeffM x x x y y y z z z
mk3x1DiffCoeff x y z = do
  x' <- x
  y' <- y
  z' <- z
  mk3x1DiffCoeffM x' y' z'
  
mk2x2DiffCoeffM x1 x2 y1 y2 = mk3x3DiffCoeffM x1 x2 zero y1 y2 zero zero zero zero
mk2x2DiffCoeff x1 x2 y1 y2 = do
  x1' <- x1
  x2' <- x2
  y1' <- y1
  y2' <- y2
  mk2x2DiffCoeffM x1' x2' y1' y2'


mkScalarDiffCoeffM x = mk3x1DiffCoeffM x x x
mkScalarDiffCoeffX x = Diffusion x x x x x x x x x 
mkScalarDiffCoeff x = do
  x' <- x
  mkScalarDiffCoeffM x'
  
singleCoeff :: Diffusion -> U.RealExpression  
singleCoeff (Diffusion x1 x2 x3 y1 y2 y3 z1 z2 z3) = x1 

-- means the realexpression has units M/s


-- the diffusion co-efficent needs to be in the process



-- This works fine
diffCoeffMultiply :: Int -> Diffusion -> Partial -> U.RealExpression
diffCoeffMultiply dimension (Diffusion x1 x2 x3 y1 y2 y3 z1 z2 z3) (Partial x y z)  = case dimension 
                                                                                     of 1 -> x1 `U.Times` x 
                                                                                        
                                                                                        2 -> (x1 `U.Times` x)`U.Plus`(x2 `U.Times` y) `U.Plus` (y1 `U.Times` x)`U.Plus` (y2 `U.Times` y)


                                                                                        3 -> (x1 `U.Times` x)`U.Plus`(x2 `U.Times` y)`U.Plus`(x3 `U.Times` z)`U.Plus`
                                                                                              (y1 `U.Times` x)`U.Plus`(y2 `U.Times` y)`U.Plus`(y3 `U.Times` z)`U.Plus`
                                                                                               (z1 `U.Times` x)`U.Plus`(z2 `U.Times` y)`U.Plus`(z3 `U.Times` z)
                                                                                        e -> error $ "invalid dimension of diffusion. You entered: " ++ (show e) ++ ". Please choose 1, 2 or 3"



-- Transforms the ReactionModel to a U.Modelbuilder
-- add in feature to check dimensionality of diffusion 
reactionModelToUnits :: Monad m => Int -> ReactionModel -> U.ModelBuilderT m (M.Map Entity U.RealExpression, M.Map Process U.RealExpression)
reactionModelToUnits dimension m = do
  --transform entities to expressions
  -- may need to add entities to the processes or reaction model.
  
  
  --builds a list of entities from entity variables
  --swapped M.values with snd
  --  let entityVars = S.toList . S.unions $ map (S.fromList . mapValue . entityVariables) (allProcesses m)
  let entityVars = S.toList . S.unions $ map ( S.fromList . M.elems . entityVariables) (allProcesses m)
  
  zeroInitialCondition <- U.realConstant U.askTimeUnits 0 .==. U.timeM
  perTime <- U.askTimeUnits $**$ (-1)
  let lSqPerT = (U.askXUnits `U.unitsPowX` 2) `U.unitsTimesX` (U.askTimeUnits `U.unitsPowX` (-1))
  diff <- (mkScalarDiffCoeff (U.realConstant lSqPerT 0))
  
  
  -- void entity instances lookups go to zero, can build the defualt diffusion co -efficient here
  let eis = flip map entityVars $ \e@(Entity u _) ->
                case M.lookup e (entityInstances m)
                of
                  Just ei -> (e, ei) -- to just return ei or a tuple with e and ei
                  -- need to set the initial value to zero for entities that do not have an instance
                  -- Boundary condition LHS RHS
                  -- the LHS really doesnt matter because it is never used.. it is simply replaced by the entity later on
                  Nothing -> (e, EntityFromProcesses (Boundary zeroInitialCondition U.TimeE (U.realConstantE u 0) )
                                                                         (U.realConstantE (u `U.unitsTimes` perTime) 0) diff )
  let eiMap = M.fromList eis

  

  
  
  
  
  

  --Each Entity gets a corresponding variable in the model, the type of entityToExpression is a map
  --from the entity to its actually UnitsModebuilder ID
  --forM performs the monadic do function on all entities on the list
      
  entityToExpression <-
    liftM M.fromList $ forM entityVars $ \e@(Entity u eid) -> do  
      let entityName = fromMaybe (shows eid "Unnamed Entity") . liftM read $ getAnnotationFromModel m e "nameIs"
      let vname = showString "Amount of " entityName -- removed the part that shows compartment
      v <- U.newNamedRealVariable (return u) vname
      return (e, U.RealVariableE v)   --assuming what is returned is a list of entities mapped to their IDs


  -- forM performs this on all lists, Since we are now looking at individual processes i need to sepciafically target the entities inside of the
      --specific process im looking in.
  procToRateEx <-
    liftM M.fromList $ forM (allProcesses m) $ \p -> do 
      v <- U.newRealVariable (U.askTimeUnits $**$ (-1))
      let vex = U.RealVariableE v
      vex `U.newEqM` (substituteRateTemplate entityToExpression p)
      return (p,vex)
        
  let fluxMap = buildFluxMap (allProcesses m) procToRateEx
      -- Each CE / EntityInstance also has an equation...
  forM_ eis $ \(e, ei) -> do
    let vex = fromJust $ M.lookup e entityToExpression
    case ei
      of
          EntityClamped ex -> vex `U.newEqM` ex
          
          EntityFromProcesses (Boundary condition leftHandSide rightHandSide) rateex coefficient -> do
     
              U.newBoundaryEq (return condition) (return vex) (return rightHandSide) --return is nessecary for newBoundaryEq
      
              
              -- check if entity exists in the fluxmap, if it doesnt exists only add the rate expression, otherwise also add the fluxes of the entity
              let rate = case M.lookup e fluxMap
                         of
                           Nothing -> rateex
                           Just fluxes -> fluxes `U.Plus` rateex
                         
              -- the diffusion coeff depends on the entity not the process
              -- not sure how well this new Divergence thing will work, may need to fix it              
                           
              --let divinput =  U.unitsAssertionM  ( U.askXUnits `U.unitsTimes` (U.askTimeUnits `U.unitsPow` (-1)) (diffCoeffMultiply dimension coefficient (grad vex))            
              --(U.PartialTime vex) `U.newEqM` ( (U.Divergence divinput)  `U.Plus` rate) 
              --(U.PartialTime vex) `U.newEqM` ( (U.getUnits (diffCoeffMultiply dimension coefficient (grad vex))) `U.Plus` rate) 
              (U.PartialTime vex) `U.newEqM`  ((U.Divergence (diffCoeffMultiply dimension coefficient (grad vex))) `U.Plus` rate)
             -- let lSqPerT = (U.askXUnits `U.unitsPowX` 2) `U.unitsTimesX` (U.askTimeUnits `U.unitsPowX` (-1)) 
              
              --S.modify $ \m->m{entityInstances EntityFromProcess _ _ coeff = M.insert e' ei' (entityInstances m)}
              
              -- need to find out why coeff is not giving the right units
             -- (U.PartialTime vex) `U.newEqM` ( (U.getUnits (singleCoeff coefficient) `U.Plus` rate) )
              
              
              --(U.PartialTime vex) `U.newEqM`  (U.getUnits (singleCoeff (mkScalarDiffCoeff (U.realConstant lSqPerT 2))) `U.Plus` rate) 
              
  return (entityToExpression,procToRateEx)          
                    
--Functions for substituting placeholders for the actual variable IDs
flipPair (a, b) = (b, a)

transposeMap :: (Ord k, Ord a) => M.Map k a -> M.Map a k
transposeMap = M.fromList . map flipPair . M.toList
composeMapsDefault :: (Ord k, Ord a) => (k -> b) -> M.Map k a -> M.Map a b -> M.Map k b
composeMapsDefault f m1 m2 = M.mapWithKey (\k a -> fromMaybe (f k) (M.lookup a m2)) m1
substituteRateTemplate ceToEx p =
  let
      varSub = composeMapsDefault (\(U.RealVariable u _) -> U.realConstantE u 0) (entityVariables p) ceToEx
  in
    {- DT.trace ((showString "Substituting rate template: Var->Var Map (" . shows varSub .
showString "), CE->Var map: (" . shows (entityVariables p) .
showString "), Var->CE map: (" .
shows ceToEx . showString "), Original " .
shows (rateTemplate p)) "") $! -}
      everywhere (mkT $ substituteOneVariable varSub) (rateTemplate p)

substituteOneVariable :: M.Map U.RealVariable U.RealExpression -> U.RealExpression -> U.RealExpression
substituteOneVariable varSub (U.RealVariableE v) =
    case (M.lookup v varSub)
      of
        Just ex -> ex
        Nothing -> U.RealVariableE v
substituteOneVariable _ ex = ex

forFoldl' s0 l f = foldl' f s0 l
alterWith m k f = M.alter f k m

buildFluxMap :: [Process] -> M.Map Process U.RealExpression -> M.Map Entity U.RealExpression
buildFluxMap processes procToVar = buildFluxMap' processes procToVar M.empty
buildFluxMap' [] _ m = m
buildFluxMap' (p:processes) procToEx m =
    let
        procEx = procToEx!p
        m' = forFoldl' m (M.toList $ stoichiometry p) $ \m0 (ce, (mup, u)) ->
               let rhs = procEx `U.Times` (U.realConstantE u mup)
                 in
                   alterWith m0 ce $ \re0 ->
                     case re0
                       of
                         Nothing -> Just rhs
                         Just re -> Just $ re `U.Plus` rhs
    in
      buildFluxMap' processes procToEx m'





--essential class and type definitions for running monads

newtype ModelBuilderT m a = ModelBuilderT (S.StateT ReactionModel (U.ModelBuilderT m) a)
type ModelBuilder = ModelBuilderT I.Identity

modelBuilderTToState (ModelBuilderT a) = a

instance Monad m => Monad (ModelBuilderT m)
    where
      (ModelBuilderT a) >>= b = ModelBuilderT $ a >>= (modelBuilderTToState . b)
      return a = ModelBuilderT (return a)
      fail a = ModelBuilderT (fail a)
instance M.MonadTrans ModelBuilderT
    where
      lift a = ModelBuilderT $ M.lift $ M.lift a
instance Monad m => S.MonadState ReactionModel (ModelBuilderT m)
    where
      get = ModelBuilderT $ S.get
      put = ModelBuilderT . S.put
instance Monad m => R.MonadReader ReactionModel (ModelBuilderT m)
    where
      ask = ModelBuilderT $ S.get
      local f (ModelBuilderT mym) = ModelBuilderT $ do
        mod <- S.get
        M.lift $ S.evalStateT mym mod

class ReactionModelBuilderAccess m m1 | m -> m1
    where
      liftReactions :: ModelBuilderT m1 a -> m a
instance ReactionModelBuilderAccess (ModelBuilderT m) m
    where
      liftReactions = id
instance Monad m1 => U.UnitsModelBuilderAccess (ModelBuilderT m1) m1
    where
      liftUnits = ModelBuilderT . M.lift
instance Monad m1 => U.UnitsModelBuilderAccess (S.StateT s (ModelBuilderT m1)) m1
    where
      liftUnits = M.lift . U.liftUnits


runModelBuilderT :: Monad m => ModelBuilderT m a -> U.ModelBuilderT m (a, ReactionModel)
runModelBuilderT = flip S.runStateT emptyReactionModel . modelBuilderTToState
execModelBuilderT :: Monad m => ModelBuilderT m a -> U.ModelBuilderT m ReactionModel
execModelBuilderT = flip S.execStateT emptyReactionModel . modelBuilderTToState
runReactionBuilderInUnitBuilder :: Monad m => Int -> ModelBuilderT m a -> U.ModelBuilderT m a
runReactionBuilderInUnitBuilder dimension mb = do
  (a, m) <- runModelBuilderT mb
  _ <- reactionModelToUnits dimension m
  return a

runReactionBuilderInUnitBuilder' :: Monad m => Int -> ModelBuilderT m a -> U.ModelBuilderT m (M.Map Entity U.RealExpression, M.Map Process U.RealExpression, a)
runReactionBuilderInUnitBuilder' dimension mb = do
  (a, m) <- runModelBuilderT mb
  (ce2v, p2v) <- reactionModelToUnits dimension m
  return (ce2v, p2v, a)

insertContextTag typetag tag v = S.modify (\m -> m {contextTaggedIDs = M.insert (typetag, tag) v (contextTaggedIDs m)})
getContextTag :: R.MonadReader ReactionModel m => D.TypeCode -> D.TypeCode -> m (Maybe Int)
getContextTag typetag tag = do
  idmap <- R.asks contextTaggedIDs
  return $ M.lookup (typetag, tag) idmap
  
contextTaggedID typetag tag wrap allocm =
    do
      t <- getContextTag typetag tag
      case t
        of
          Just id -> return $ wrap id
          Nothing ->
            do
              id <- allocateID
              allocm id
              insertContextTag typetag tag id
              return $ wrap id
              
queryContextTaggedID typetag tag wrap =
  (liftM . liftM) wrap (getContextTag typetag tag)

allocateID :: S.MonadState ReactionModel m => m Int
allocateID = S.modify (\m -> m { nextID = (+1) $ nextID m}) >> (S.gets $ flip (-) 1 . nextID)

annotateModel :: (Show a, Show b, Show c, Monad m) => a -> b -> c -> ModelBuilderT m ()
annotateModel s p o = S.modify (\m -> m { annotations = M.insert ((show s), (show p)) (show o) (annotations m) })
getAnnotation :: (Show a, Show b, Monad m) => a -> b -> ModelBuilderT m (Maybe String)
getAnnotation s p = do
  am <- S.gets annotations
  return $ M.lookup (show s, show p) am

requireNameM :: (Monad m, Show a) => ModelBuilderT m a -> String -> ModelBuilderT m a
requireNameM m n = do
    m' <- m
    annotateModel m' "nameIs" n
    return m'

getAnnotationFromModel m s p = M.lookup (show s, show p) (annotations m)


type ProcessBuilderT m a = S.StateT Process (ModelBuilderT m) a
instance Monad m1 => ReactionModelBuilderAccess (S.StateT Process (ModelBuilderT m1)) m1
    where
      liftReactions = M.lift

--build boundary conditions
boundaryConditionM :: Monad m =>  U.RealExpression -> U.RealExpression -> U.ModelBuilderT m U.BoolExpression
boundaryConditionM lhs rhs = return $ U.Equal lhs rhs       
boundaryCondition :: Monad m => U.ModelBuilderT m U.RealExpression -> U.ModelBuilderT m U.RealExpression -> U.ModelBuilderT m U.BoolExpression
boundaryCondition lhs rhs = do       
  lhs' <- lhs
  rhs' <- rhs
  boundaryConditionM lhs' rhs'
      

boundaryEquationM :: Monad m => U.BoolExpression -> U.RealExpression -> U.RealExpression -> U.ModelBuilderT m Boundary
boundaryEquationM boundarycondition lhs rhs = return $ Boundary boundarycondition lhs rhs
boundaryEquation :: Monad m => U.ModelBuilderT m U.BoolExpression -> U.ModelBuilderT m U.RealExpression -> U.ModelBuilderT m U.RealExpression -> U.ModelBuilderT m Boundary
boundaryEquation boundarycondition lhs rhs = do
  boundarycondition' <- boundarycondition
  lhs' <- lhs
  rhs' <- rhs
  boundaryEquationM boundarycondition' lhs' rhs'



--building a new process  takes a ProcessBuilderT and makes it into a ModelBuilderT 
newProcess :: Monad m => ProcessBuilderT m a -> ModelBuilderT m Process
newProcess pb = do 
  id <- allocateID
  let p0 = Process { processId = id,
                     entityVariables = M.empty,
                     stoichiometry = M.empty,
                     rateTemplate = U.realConstantE U.dimensionlessE 0
                   }
  p1 <- S.execStateT pb p0
  S.modify (\m -> m{allProcesses = p1:(allProcesses m)})
  return (p1)                                                             -- not sure about what to return here
  
addEntity :: Monad m => Double -> ModelBuilderT m Entity -> ProcessBuilderT m (U.ModelBuilderT m U.RealExpression)
addEntity  stoich me = do      --me = modelBuilder Entity
  e@(Entity u eid) <- M.lift me
  v <- U.liftUnits $ U.newRealVariable (return u)
  {- DT.trace ((showString "Added a temporary variable: " . shows (U.variableId v)) "") $ return () -}
 
  S.modify (\p -> p{entityVariables = M.insert v e $ entityVariables p, stoichiometry = M.insert e (stoich, u) (stoichiometry p)})
  return $ return (U.RealVariableE v)




--puts the rate equation into the ProcessBuilderT rateTemplate  
rateEquation :: Monad m => U.ModelBuilderT m U.RealExpression -> ProcessBuilderT m ()
rateEquation rm = do
  r <- U.liftUnits rm
  S.modify (\p->p{rateTemplate=r})

data EntityTag = EntityTag deriving (D.Typeable, D.Data)
entityTypeTag = D.typeCode EntityTag

newEntity :: Monad m => U.ModelBuilderT m U.Units -> ModelBuilderT m Entity
newEntity u = do
  id <- allocateID
  u' <- U.liftUnits u
  return $ Entity u' id

newTaggedEntity :: Monad m => U.ModelBuilderT m U.Units -> D.TypeCode -> ModelBuilderT m Entity
newTaggedEntity u tag = do
  u' <- U.liftUnits u
  contextTaggedID entityTypeTag tag (Entity u') return
  
newNamedEntity :: Monad m => U.ModelBuilderT m U.Units -> String -> ModelBuilderT m Entity
newNamedEntity u = requireNameM (newEntity u)
newNamedTaggedEntity :: Monad m => U.ModelBuilderT m U.Units -> D.TypeCode -> String -> ModelBuilderT m Entity
newNamedTaggedEntity u t = do
  requireNameM (newTaggedEntity u t)


addEntityInstance :: Monad m => ModelBuilderT m Entity -> ModelBuilderT m EntityInstance -> ModelBuilderT m ()
addEntityInstance e ei = do
  e' <- e
  ei' <- ei
  S.modify $ \m->m{entityInstances=M.insert e' ei' (entityInstances m)}
  

stoichiometryNoUnits = S.fromList . map (\(e, (v, _)) -> (e, v)) . M.toList . stoichiometry






-- Also provide some Template Haskell utilities for declaring tagged Entities & Compartments...

-- | Declares a named, tagged entity for use from multiple contexts, using
-- | Template Haskell. Note that the first expression argument should have type
-- | Monad m => ModML.Units.UnitsDAEModel.ModelBuilderT m Units
-- | In addition, the qualified namespace of
declareNamedTaggedEntity :: T.Q T.Exp -> String -> String -> T.Q [T.Dec]
declareNamedTaggedEntity expr prettyName varName = do
  expr' <- expr
  let applyUnitsName = T.AppE (T.AppE (T.AppE (T.VarE $ T.mkName "R.newNamedTaggedEntity") expr') $
                                T.VarE $ T.mkName (varName ++ "Tag"))
                         (T.LitE (T.StringL prettyName))
  U.declareTaggedSomething applyUnitsName varName

declareNamedTaggedCompartment :: String -> String -> T.Q [T.Dec]
declareNamedTaggedCompartment = U.declareNamedTaggedSomething "R.newNamedTaggedCompartment"