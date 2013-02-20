{-# LANGUAGE BangPatterns,RankNTypes,DeriveDataTypeable #-}
module Pdesolver
    (
     CodeGenerationError (OtherProblem),
     AllowCodeGenError,
     IntegrationResult (FatalError, Warning, CheckedConditionFail, Result, Success),
     SolverParameters (SolverParameters),
     defaultSolverParameters,
     modelToResults,
     makeCodeFor,
     tStart,
     maxSolverStep,
     maxReportStep,
     tEnd,
     showEveryStep,
     reltol,
     abstol,
     variableOverrides
    )
where

import Pdecore
import Control.Monad
import Control.Monad.Error
import Data.List hiding ((\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set ((\\))
import Data.Generics
import Control.Monad.State
import Data.Maybe
--import Text.ParserCombinators.Parsec
import qualified System.IO as S
import qualified System.IO.Unsafe as S
import qualified System.Process as S
import System.FilePath
import System.Process
import System.Exit
import qualified Control.Exception as S
import Numeric
import Data.Ord
import Data.Maybe
import System.Directory
import Control.Concurrent
import Control.Concurrent.MVar


--need to write code that will read and write. something like a main that pulls everything together


--calls other functions to translate                              
makeCodeFor :: BasicPDEModel ->  String
makeCodeFor mod =
    let mod' = removeUnusedVariablesAndCSEs (simplifyMaths mod)
    in  "from fipy import * " ++
                (buildBoundaries mod')  ++ "\n\n" ++
                (createFipyTerms (makeEqCoeffList mod')) 
                
                --makeEdCoeffList takes a model, takes all the coefficients for diffusion term, convection term, transient term and source for each equation and stores them as a map
                -- the key to the maps for diffusionterm are for example  (dy dx v) , where dy and dx are the partial terms and v is the variable. To describe each term there is a map for each term 
                -- in every equation.. eventually a list of these maps are created [ (transient, diffusion, convection, source), ......., ..........].  fold through this list to extract the coefficients
                -- for the terms and convert them into strings to create the appropriate fipy terms. there are currently bugs that need to be resolved in the conversion to string process.
                
                
                --solver parameters.. timesteping and sweeping needs to be done
                -- figures need to be plotted if possible

      
    
-- code that calls functions that remove stuff like RealExpression... etc    
simplifyMaths mod@(BasicPDEModel {equations = eqns, boundaryEquations = beqns, interventionRoots = ivrs,
                                  forcedInequalities = fieqs, checkedConditions = cconds }) =
  mod { equations =
           map (\(RealEquation rex1 rex2) -> RealEquation (simplifyRealExpression rex1) (simplifyRealExpression rex2) ) eqns,
        boundaryEquations =
          map (\(bex, RealEquation rex1 rex2) ->
                (bex, RealEquation (simplifyRealExpression rex1) (simplifyRealExpression rex2))) beqns,
        interventionRoots =
          map simplifyRealExpression ivrs,
        forcedInequalities = map simplifyRealExpression fieqs,
        checkedConditions = map (\(str,bex) -> (str, simplifyBoolExpression bex)) cconds
      }
  
 
removeUnusedVariablesAndCSEs mod =
  let
   removeCommonSubexpression (RealCommonSubexpression _ ex) = ex
   removeCommonSubexpression (BoolCommonSubexpression _ ex) = ex 
  in
   -- does this count at (a->a)??
   everywhere (mkT removeCommonSubexpression) mod
                           
-- cannot handle any other time constraints other than initial conditions                           



isInitial ( (Equal TimeE (RealConstant 0)) , RealEquation rex1 rex2 ) = True
isInitial ( (Equal (RealConstant 0) TimeE) , RealEquation rex1 rex2 ) = True                                          
isInitial _ = False
isInitial' :: [(BoolExpression,RealEquation)] -> ([(BoolExpression,RealEquation)],[(BoolExpression,RealEquation)])
isInitial'  = partition isInitial 
    
isVariable :: RealExpression -> Bool
isVariable (RealVariableE a ) = True 
isVariable _ = False
--splitEquation :: [RealExpression] -> ([RealExpression],[RealExpression])
splitEquation eq = partition (everything (||) (mkQ False isVariable)) eq


buildBoundaries mod@(BasicPDEModel { boundaryEquations = beqns}) =                            
  
  let
  simplifyInitial bEquation = (foldl' (\acc beq  -> let (initialCondition, RealEquation rex1 rex2  ) = beq  
                                                        eq = [rex1,rex2]
                                                        ([variable], [rex]) = splitEquation eq
                                                        RealVariableE a = variable
                                                    in 
                                                     shows (a) . showString " = CellVariable(mesh=m, hasOld = True, value = " .
                                                     (realExpressionToString rex) $ ")"
                                      ) [] bEquation)
                              
  --currently this can only handle terms in the form RealVariable a = something.. this can be easily changed later                            
  simplifyBoundaryExpression bEquation = (foldl' (\acc  (n, (bex, RealEquation rex1 rex2 )) ->
                                                  let 
                                                    eq = [rex1,rex2]
                                                    boolToString  = boolExpressionToString bex
                                                    ([variable], [rex]) = splitEquation eq 
                                                    RealVariableE a = variable
                                                  in  
                                                   showString "mask" . shows (n) . showString " = ( " . boolToString . showString  " )\n" . 
                                                   shows (a) . showString ".constrain (" .  (realExpressionToString rex) . showString " , where=mask" . shows (n) $ ")" 
                                                 ) [] bEquation) 
                                     
  (initial,boundary) = isInitial' beqns
  numBeqns = zip [1..] boundary
  
  in
   (simplifyInitial initial) ++ "\n\n" ++ (simplifyBoundaryExpression numBeqns)
   

  
boolExpressionToString :: BoolExpression -> String -> String
boolExpressionToString (BoolConstant True) = showString "True"
boolExpressionToString (BoolConstant False) = showString "False"
boolExpressionToString  (be1 `And` be2) =
  showString "(" . boolExpressionToString  be1 .
  showString ") & (" .
  boolExpressionToString  be2 .
  showString ")"
  
-- need to change the or statement
boolExpressionToString  (be1 `Or` be2) =
  showString "(" . boolExpressionToString  be1 .
  showString ")|(" . boolExpressionToString  be2 .
  showString ")"

boolExpressionToString  (Not be1) =
  showString "not(" . boolExpressionToString  be1 . showString ")"

boolExpressionToString  (re1 `LessThan` re2) =
  showString "(" . realExpressionToString  re1 .
  showString ")<(" . realExpressionToString  re2 .
  showString ")"
boolExpressionToString  (re1 `Equal` re2) =
  showString "(" . realExpressionToString  re1 .
  showString ")==(" . realExpressionToString  re2 .
  showString ")"



-- NOTE need to set initial NO FLUX conditions.
realExpressionToString :: RealExpression -> String -> String
realExpressionToString (RealConstant c) = shows c
--realExpressionToString (RealVariableE v) = show (v)     dont need this
realExpressionToString (XSpaceE)  = showString " X "
realExpressionToString (YSpaceE)  = showString " Y "
realExpressionToString (ZSpaceE)  = showString " Z " 


--Need to figure out a way to express Partial and normal flux conditions on the boundary
realExpressionToString (PartialSpaceX (RealVariableE v)) = shows (v)
realExpressionToString (PartialSpaceY (RealVariableE v)) = shows (v)
realExpressionToString (PartialSpaceZ (RealVariableE v)) = shows (v)


realExpressionToString  (If b1 r1 r2) =
  showString "( if " .
  boolExpressionToString  b1 .
  showString ") : (" .
  realExpressionToString  r1 .
  showString ") else : (" . realExpressionToString  r2 .
  showString ")"
realExpressionToString  (r1 `Plus` r2) =
  showString "(" . realExpressionToString  r1 .
  showString ")+(" . realExpressionToString  r2 .
  showString ")"
realExpressionToString  (r1 `Minus` r2) =
    showString "(" . realExpressionToString  r1 .
    showString ")-(" .
    realExpressionToString  r2 . showString ")"
realExpressionToString  (r1 `Times` r2) =
  showString "(" . realExpressionToString  r1 .
  showString ")*(" . realExpressionToString  r2 .
  showString ")"
realExpressionToString  (r1 `Divided` r2) =
  showString "(" . realExpressionToString  r1 .
  showString ")/(" . realExpressionToString  r2 .
  showString ")"

realExpressionToString  (r1 `Power` r2) =
  showString "(" . realExpressionToString  r1 .
  showString ")**(" . realExpressionToString  r2 .
  showString ")"

--not sure about floor ad ceiling functions
-- realExpressionToString  (Floor r1) =
--   showString "floor(" . realExpressionToString  r1 . showString ")"
-- realExpressionToString  (Ceiling r1) =
--   showString "ceiling(" . realExpressionToString  r1 . showString ")"


realExpressionToString  (LogBase r1 r2) = 
  showString "numerix.log(" . realExpressionToString  r2 . showString ") / log(" .
  realExpressionToString  r1 . showString ")"
  
realExpressionToString  (Sin r1) =
  showString "numerix.sin(" . realExpressionToString  r1 .
  showString ")"
realExpressionToString  (Tan r1) =
  showString "numerix.tan(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (Cos r1) =
  showString "numerix.cos(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (ASin r1) =
  showString "numerix.arcsin(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (ATan r1) =
  showString "numerix.arctan(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (ACos r1) =
  showString "numerix.arccos(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (Sinh r1) =
  showString "numerix.sinh(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (Tanh r1) =
  showString "numerix.tanh(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (Cosh r1) =
  showString "numerix.cosh(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (ASinh r1) =
  showString "numerix.arcsinh(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (ATanh r1) =
  showString "numerix.arctanh(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (ACosh r1) =
  showString "numerix.arccosh(" . realExpressionToString  r1 . showString ")"
realExpressionToString  (RealExpressionTag _ r) =
  realExpressionToString r

    
--moves all terms to 1 side of the equation
--takes all terms and puts them into a list
equationOneSide (RealEquation a b) =  (b `Minus` a)  

eqToList' eq = eqToList eq 1
--most legit code
eqToList :: RealExpression -> Double -> [(RealExpression, Double)]
eqToList (Times (RealConstant a) re) multiplier  = eqToList  re  (a*multiplier)                                                                 
eqToList (Times re (RealConstant a)) multiplier  = eqToList  re  (a*multiplier) 

eqToList (Plus a b ) multiplier =  (eqToList a multiplier) ++ (eqToList b multiplier) 
eqToList (Minus  a b ) multiplier =  (eqToList a multiplier) ++ (eqToList b ((-1)*multiplier)) 

                                                  
eqToList (PartialSpaceX a) multiplier =  [(PartialSpaceX a, multiplier)] 
eqToList (PartialSpaceY a) multiplier =  [(PartialSpaceY a, multiplier)]
eqToList (PartialSpaceZ a) multiplier =  [(PartialSpaceZ a, multiplier)]
eqToList (RealVariableE a)  multiplier = [(RealVariableE a, multiplier)]
eqToList (RealConstant a) multiplier = [(RealConstant a, multiplier)]
eqToList (PartialTime a) multiplier =  [(PartialTime a, multiplier)] 
eqToList e multiplier = error $ "invalid term " ++ (show e) ++ " for solver."


-- partial element is made as a key
data PartialElement = Dx | Dy | Dz | Dt deriving (Eq, Ord, Show)
-- data SourceElement =  Constant | Variable  deriving (Eq, Ord, Show)  <--- no longer required

--added a realVariable so i know which variable each matrix of co efficients coresponds to
data DiffusionState = DiffusionState { 
  secondOrderCoefficients :: M.Map (PartialElement, PartialElement, RealExpression) Double
}

data FirstorderState = FirstorderState {
  firstOrderCoefficients :: M.Map (PartialElement, RealExpression) Double
}

--NOTE doesnt handle terms involve sin etc.. it only handles realconstants ... not complicated terms
data SourceTermState = SourceTermState {
  sourceCoefficients :: M.Map (RealExpression) Double    -- changed from map of source element to a RealExpression.. problem with redefining realconstant
}

--initialize the maps
emptyDiffusion = DiffusionState {secondOrderCoefficients = M.empty}
emptyFirstOrder = FirstorderState {firstOrderCoefficients = M.empty}
emptySource = SourceTermState {sourceCoefficients = M.empty}
                                                                    
-- making Diffusion terms....  pattern match the terms and store in map
makeDiffusion oldstate ( (PartialSpaceX (PartialSpaceX (a))), coefficient ) =  
  oldstate { secondOrderCoefficients = M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dx,Dx,a) (secondOrderCoefficients oldstate)}
  
makeDiffusion oldstate ( (PartialSpaceX (PartialSpaceY (a))), coefficient ) =  
  oldstate { secondOrderCoefficients = M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dx,Dy,a) (secondOrderCoefficients oldstate)}
  
makeDiffusion oldstate ( (PartialSpaceX (PartialSpaceZ (a))), coefficient ) =  
  oldstate { secondOrderCoefficients = M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dx,Dz,a) (secondOrderCoefficients oldstate)}
  
makeDiffusion oldstate ( (PartialSpaceY (PartialSpaceX (a))), coefficient ) =  
  oldstate { secondOrderCoefficients = M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dy,Dx,a) (secondOrderCoefficients oldstate)}
  
makeDiffusion oldstate ( (PartialSpaceY (PartialSpaceY (a))), coefficient ) =  
 oldstate { secondOrderCoefficients =  M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dy,Dy,a) (secondOrderCoefficients oldstate)}
  
makeDiffusion oldstate ( (PartialSpaceY (PartialSpaceZ (a))), coefficient ) =  
 oldstate { secondOrderCoefficients =  M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dy,Dz,a) (secondOrderCoefficients oldstate)}
  
makeDiffusion oldstate ( (PartialSpaceZ (PartialSpaceX (a))), coefficient ) =  
 oldstate { secondOrderCoefficients =  M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dz,Dx,a) (secondOrderCoefficients oldstate)}
  
makeDiffusion oldstate ( (PartialSpaceZ (PartialSpaceY (a))), coefficient ) =  
 oldstate { secondOrderCoefficients =  M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dz,Dy,a) (secondOrderCoefficients oldstate)}
  
makeDiffusion oldstate ( (PartialSpaceZ (PartialSpaceZ (a))), coefficient ) =  
 oldstate { secondOrderCoefficients =  M.update (\oldcoeff-> Just (oldcoeff + coefficient)) (Dz,Dz,a) (secondOrderCoefficients oldstate)}

--do something similar for the convetion terms
makeConvection oldstate ( (PartialSpaceX (a)), coefficient ) =
  oldstate { firstOrderCoefficients = M.update (\oldcoeff -> Just (oldcoeff + coefficient)) (Dx,a) (firstOrderCoefficients oldstate)}
makeConvection oldstate ( (PartialSpaceY (a)), coefficient ) =
  oldstate { firstOrderCoefficients = M.update (\oldcoeff -> Just (oldcoeff + coefficient)) (Dy,a) (firstOrderCoefficients oldstate)}
makeConvection oldstate ( (PartialSpaceZ (a)), coefficient ) =
  oldstate { firstOrderCoefficients = M.update (\oldcoeff -> Just (oldcoeff + coefficient)) (Dz,a) (firstOrderCoefficients oldstate)}

makeTransient oldstate ( (PartialTime (a)), coefficient ) =
  oldstate { firstOrderCoefficients = M.update (\oldcoeff -> Just (oldcoeff + coefficient)) (Dt,a) (firstOrderCoefficients oldstate)}  
makeSource oldstate ( (RealConstant (a)), coefficient ) =
  oldstate { sourceCoefficients = M.update (\oldcoeff -> Just (oldcoeff + coefficient)) (RealConstant  a) (sourceCoefficients oldstate)}
makeSource oldstate ( (RealVariableE (a)), coefficient ) =
  oldstate { sourceCoefficients = M.update (\oldcoeff -> Just (oldcoeff + coefficient)) (RealVariableE a) (sourceCoefficients oldstate)}
                                      

  
--makeEqCoeffList :: BasicPDEModel -> [  ]
makeEqCoeffList m@(BasicPDEModel { equations = eqns, forcedInequalities = ieqs }) =
    let
       
      
      diffusionTerm equation = foldl' (\state term -> makeDiffusion state term) emptyDiffusion equation
      convectionTerm equation = foldl' (\state term -> makeConvection state term) emptyFirstOrder equation
      sourceTerm equation = foldl' (\state term -> makeSource state term) emptySource equation
      transientTerm equation = foldl' (\state term -> makeTransient state term) emptyFirstOrder equation
      oneSidedExpression = (map (eqToList'. equationOneSide) eqns)
    
    in
     -- fold through each equation, processing all the terms and storing them each as a map. Evntually a list of maps is created
     foldl' (\acc equation -> acc ++ [(transientTerm equation,
                                               diffusionTerm equation, 
                                               convectionTerm equation,   
                                               sourceTerm equation)]  ) [] oneSidedExpression
     
createFipyTerms eqCoeffList =
  
  --conversion of the list of maps into strings.... bugs to be fixed here
  -- I wonder if you can have more than 1 transient term per equation?.. it doesnt make much sense to have more than 1 transient term
 foldl' (\acc (equation, eqNum) -> showString "eq" . shows (eqNum). showString  " = " .  
                                   shows (transientToString equation) . showString " == " .  
                                   shows (diffusionToString equation) . showString " + " . 
                                   shows (convectionToString equation) . showString " + " . shows (sourceToString equation) $ "\n")
 [] (zip eqCoeffList [1..])
                             
                             
                             
                             
  
-- perform a fold looking over all the variables in the equation and then from that we can perform a 
-- map over those variables.

third (_,_,a) = a 
second (_,a) = a

--shortcuts for making commas and brackets
sc = showString ", " 
sb = showString "]"
sob = showString "["



---MAIN BUGGS BELOW THIS POINT
--function to create the Fipy diffusion term
diffusionToString [( _, diffusion, _, _ )] = 
  
  --mlu - maplookup
  let mlu a = diffusion M.! a 
  in 
   let variables = map third (M.keys diffusion)  --pick out the variables but looking at the third element of the keys
   in
    --fold through the coefficient for each of the variables and create the Fipy "DiffusionTerm" with the valid diffusion coefficients
    -- this builds a list of "DiffusionTerm[coeff]" for each variable
    -- since its a string the ++ operator will append a string to the end of another
    (foldl' (\varEquation  var  -> 
             varEquation . showString " + DiffusionTerm ([[[" .  
             shows ( mlu (Dx, Dx, var)) . sc . shows ( mlu (Dx, Dy, var)) . sc . shows ( mlu (Dx, Dz, var)) . sb . sc . 
             sob . shows ( mlu (Dy, Dx, var)) . sc . shows ( mlu (Dy, Dy, var)) . sc . shows ( mlu (Dy, Dz, var)) . sb . sc . 
             sob . shows ( mlu (Dz, Dx, var)) . sc . shows ( mlu (Dz, Dy, var)) . sc . shows ( mlu (Dz, Dz, var)) 
             . sb . sb . sb $ "")
    (showString "0") variables)
    
    

convectionToString [( _, _ ,convection, _ )] =
  
  let mlu a = convection M.! a
  in 
   let variables = map second (M.keys convection)
   in 
    foldl' (\varEquation var -> 
            varEquation . showString " + ConvectionTerm ([[[" . 
            shows (mlu (Dx, var)) . sc . shows (mlu (Dy, var)) . sc . shows (mlu (Dz, var)) . sb . sb . sb $ "")
    (showString "0") variables
  


transientToString [( transient, _ , _ , _ )] =
  
  let mlu a = transient M.! a
  in 
   let variables = map second (M.keys transient)
   in
    foldl' (\varEquation var -> 
            varEquation . showString " + TransientTerm (" . 
            --transient term needs to be multiplied by -1 because the transient term needs to be moved to the LHS, (everything is currently on the RHS)
            shows ((-1)*(mlu (Dt, var)) ) ")" $ "")
    (showString "0") variables
  
filterConstant (RealConstant, a ) =  a 
filterConstant (RealVariableE, a ) = []
filterVariable (RealVariableE, a ) = a
filterVariable (RealConstant, a ) = []    

sourceToString [( _ , _ , _ , source )] =
  
  let mlu a = source M.! a
  in 
   -- need to seperate the realvaraible and realconstant source terms before processing
   
   let variables = map filterVariable (M.keys source) 
   in 
    let constants = map filterConstant (M.keys source) 
    in 
     (foldl' (\varEquation var -> 
               varEquation . showString "(" . 
               shows (mlu (RealVariableE, var)) $ ") + " ) 
     (showString "0") variables) 
     ++ 
     (foldl' (\varEquation var -> 
               varEquation . showString " + (" . 
               shows (mlu (RealConstant, var)) $ ")" 
     (showString "0") constants))
  
  
     
--conversion from list of maps to string ends here...      
------------------------------------------------------------------------------------     
     
-- these functions should work fine     
     
unaryEvaluateOrPassthrough c f rex =
  case (simplifyRealExpression rex)
  of
    RealConstant rc -> RealConstant (f rc)
    rex' -> c rex'
    
  
-- here is where i can take advantage of the partial terms and co efficient stuff
simplifyRealExpression ex@(RealConstant _) = ex
simplifyRealExpression ex@(RealVariableE _) = ex
simplifyRealExpression ex@BoundVariableE = ex
simplifyRealExpression (Derivative ex) = Derivative (simplifyRealExpression ex)
simplifyRealExpression (RealCommonSubexpressionE (RealCommonSubexpression id ex)) =
  case simplifyRealExpression ex
  of
    rc@(RealConstant _) -> rc
    rce@(RealCommonSubexpressionE (RealCommonSubexpression _ _)) -> rce
    rv@(RealVariableE _) -> rv
    ex' -> RealCommonSubexpressionE (RealCommonSubexpression id ex')
simplifyRealExpression (If bex rex1 rex2) =
  let
    bex' = simplifyBoolExpression bex
    rex1' = simplifyRealExpression rex1
    rex2' = simplifyRealExpression rex2
  in
   case bex'
   of
     BoolConstant True -> rex1'
     BoolConstant False -> rex2'
     _ | rex1' == rex2' -> rex1'
     otherwise -> If bex' rex1' rex2'
     
simplifyRealExpression (Plus rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant 0, rex') -> rex'
    (rex', RealConstant 0) -> rex'
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 + rc2)
    (rex1', rex2') -> Plus rex1' rex2'
            
simplifyRealExpression (Minus rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (rex', RealConstant 0) -> rex'
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 - rc2)
    (rex1', rex2') -> Minus rex1' rex2'
   
simplifyRealExpression (Times rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant 0, _) -> RealConstant 0
    (_, RealConstant 0) -> RealConstant 0
    (RealConstant 1, rex') -> rex'
    (rex', RealConstant 1) -> rex'
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 * rc2)
    (RealConstant (-1), rex') -> (RealConstant 0 `Minus` rex')
    (rex', RealConstant (-1)) -> (RealConstant 0 `Minus` rex')
    (rex1', rex2') -> Times rex1' rex2'
   
simplifyRealExpression (Divided rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant 0, _) -> RealConstant 0
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 / rc2)
    (LogBase rex1 rex2, LogBase rex3 rex4) | rex1 == rex3 -> LogBase rex4 rex2
    (rex1', rex2') -> Divided rex1' rex2'
    
simplifyRealExpression (Power rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant 0, _) -> RealConstant 0
    (RealConstant 1, _) -> RealConstant 1
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 ** rc2)
    (rex1', rex2') -> Power rex1' rex2'
    
simplifyRealExpression (Ceiling rex1) =
  case (simplifyRealExpression rex1)
  of
    (RealConstant rc) -> RealConstant (fromIntegral $ ceiling rc)
    rex1' -> Ceiling rex1'
    
simplifyRealExpression (LogBase rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant rc1, RealConstant rc2) -> RealConstant $ logBase rc1 rc2
    (rex1', rex2') -> LogBase rex1' rex2'
      
simplifyRealExpression (Sin rex) = unaryEvaluateOrPassthrough Sin sin rex
simplifyRealExpression (Tan rex) = unaryEvaluateOrPassthrough Tan tan rex
simplifyRealExpression (Cos rex) = unaryEvaluateOrPassthrough Cos cos rex
simplifyRealExpression (ASin rex) = unaryEvaluateOrPassthrough ASin asin rex
simplifyRealExpression (ATan rex) = unaryEvaluateOrPassthrough ATan atan rex
simplifyRealExpression (ACos rex) = unaryEvaluateOrPassthrough ACos acos rex
simplifyRealExpression (Sinh rex) = unaryEvaluateOrPassthrough Sinh sinh rex
simplifyRealExpression (Tanh rex) = unaryEvaluateOrPassthrough Tanh tanh rex
simplifyRealExpression (Cosh rex) = unaryEvaluateOrPassthrough Cosh cosh rex
simplifyRealExpression (ASinh rex) = unaryEvaluateOrPassthrough ASinh asinh rex
simplifyRealExpression (ATanh rex) = unaryEvaluateOrPassthrough ATanh atanh rex
simplifyRealExpression (ACosh rex) = unaryEvaluateOrPassthrough ACosh acosh rex
simplifyRealExpression (RealExpressionTag s ex) = RealExpressionTag s (simplifyRealExpression ex)

simplifyBoolExpression (BoolConstant b) = BoolConstant b
simplifyBoolExpression (BoolCommonSubexpressionE (BoolCommonSubexpression id bex)) =
  BoolCommonSubexpressionE (BoolCommonSubexpression id (simplifyBoolExpression bex))
simplifyBoolExpression (And bex1 bex2) =
  case (simplifyBoolExpression bex1, simplifyBoolExpression bex2)
  of
    (BoolConstant True, bex') -> bex'
    (BoolConstant False, _) -> BoolConstant False
    (bex', BoolConstant True) -> bex'
    (_, BoolConstant False) -> BoolConstant False
    (bex1', bex2') -> And bex1' bex2'
simplifyBoolExpression (Not bex) =
  case (simplifyBoolExpression bex)
  of
    Not bex' -> bex'
    BoolConstant bc -> BoolConstant (not bc)
    bex' -> Not bex'
simplifyBoolExpression (Or bex1 bex2) =
  case (simplifyBoolExpression bex1, simplifyBoolExpression bex2)
  of
    (BoolConstant True, _) -> BoolConstant True
    (BoolConstant False, bex') -> bex'
    (_, BoolConstant True) -> BoolConstant True
    (bex', BoolConstant False) -> bex'
    (bex1', bex2') -> Or bex1' bex2'
simplifyBoolExpression (LessThan rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant rc1, RealConstant rc2) -> BoolConstant (rc1 < rc2)
    (rex1', rex2') -> LessThan rex1' rex2'
simplifyBoolExpression (Equal rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant rc1, RealConstant rc2) -> BoolConstant (rc1 == rc2)
    (rex1', rex2') -> Equal rex1' rex2'
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
                             
                           
--current boundary conditions can only handle that in the statement for declaring the realvariable, the realvariable is just a single variable.                           
-- remember to set general no flux condition                           
-- buildBoundary mod@(Pdemodel { boundaryEquations = beqns}) =   
--   let 
--     isVariable (RealVariableE a ) = true 
--     splitEquation eq = partition (everything (||) (mkQ isVaraible) eq ) 
    
--     simplifyBoundaryExpression bEquation = (map (\(BoolExpression bex, RealEquation rex1 rex2 ) ->
--                                                   let eq = [rex1,rex2]
--                                                       boolToString  = boolExpressionToString bex
--                                                       [(RealVariable a), rex] = splitEquation eq 
--                                                       realEqToString = (a, realExpressionToString rex)
--                                                   in (boolToString, realEqToString)) 
--                                             bEquation)
--     numBeqns = zip [1..] beqns
--   in 
   
--    --say that the problem is solved in a closed boundary... crap still need to find var...
--    --make masks
--    foldl' (\acc (n,(bEquation)) -> 
--             let 
--               (boolExpression, realEquation) = (simplifyBoundaryEquation bEquation)
--               variable = fst realEquation
--               expression = snd realEquation
--             in 
--              case boolExpression of
--                TimeI -> []
--                otherwise ->
--                  showString "mask" $ show (n) $ showString " = ( " show (boolExpression) ++ " )\n" $ 
--                  showVar (variable) $ showString ".constrain (" show (expression) $ showString" , where=mask" show (n) ++ ")" ) [] numBeqns