module Testmodel
where
import Pdecore

--testing out the PDEcore

--building basic equations


b = (RealConstant 2)

c = Minus (RealConstant 2) (RealConstant 3)

main1 = print testmodel 

--NOTES--
--newRealVariable has a double wrapping around it, therefore it must be unwrapped twice before being fed into newEq, the 
-- <- operator does the first level of unwrapping and the newEq itself pefroms another unwrapping on all its arguements.

-- things like falseM are already wrapped in ModelBuilder, thus putting them into newBoundaryEq is fine as newBoundaryEq
-- will unwrap all the arguements

testmodel :: BasicPDEModel
testmodel = buildModel $ do
  
  --creating variables... newNamedRealVariable has a double wrapper around the RealVarible that is created.
  -- in the process of creation it also changes state, by creating a Variable ID and giving it a S P O 
  
  x <- newNamedRealVariable "Bulbasaur"
  y <- newNamedRealVariable "Squirtle"  
  
  
  x2 <- newNamedRealVariable "Charmander"
  y2 <- newNamedRealVariable "Pidgey"  
  
  
  --changes state (modify) by creating an equation
  -- the .+. operator (and similar other operators) work specifically on the RealExpressions with a ModelBuilderT wrapper
  (x .+. x2) `newEq` (y .+. y2)
  
  d <- newRealVariable
  e <- newRealVariable
  
  -- so a conditonal statement is created with Realexpressions (wrapped around a ModelBuilderT)
  let boolcondition = (d.>.e .&&. x2.>.y2)
      
  
       
  newBoundaryEq boolcondition e d
  
  --need to work out how the get annotation function works.

--now we work with a PDE model
  
main2 = print pdemodel

pdemodel :: BasicPDEModel
pdemodel = buildModel $ do
  
  --let u = substance 1
  
  u <- newNamedRealVariable "substance 1"
  
  -- this is how to attach a partial term onto a constant
  let du = partialTime u
  charmander <- newNamedRealVariable "charmander"    
  
  --Equation #1 stating that the differential of substance 1 with respect to is equation to a charmander
  
  du `newEq` charmander
  
  
  -- Equation #2
  -- Attaching two partial terms in order to convert to double partial terms we must unwrap du then feed it into partialX
  -- Stating that the differential with respect to time and x for substance 1 is equal to 2 charmanders
  partialX du `newEq` (charmander .+. charmander)

  -- now we want to set an initial condition
  -- stating that at time = 0 charmander must = 4
  initialValue 0 charmander 4

-- now to create a basic example of a coupled reaction diffusion problem (assuming a constant diffusion term)

main3 = print reactionDiffusionProblem

reactionDiffusionProblem = buildModel $ do
  
  u <- newNamedRealVariable "u"
  v <- newNamedRealVariable "v"
  diffU <- newNamedRealVariable "Diffusion Constant for u"
  f <- newNamedRealVariable "Source Term" 
  diffV <- newNamedRealVariable "Diffusion Constant for v"
  k <- newNamedRealVariable "Rate at which V->P"

  -- partialX (partialX u) `newEq` u
  
  let laplacianU = (partialX (partialX u)) .+. (partialY (partialY u)) .+. (partialZ (partialZ u)) 
  let laplacianV = (partialX (partialX v)) .+. (partialY (partialY v)) .+. (partialZ (partialZ v))
  
  -- First equation    
  (diffU .*. laplacianU .-. u .*. (v .**. realConstantM 2) .+. f .*. (realConstantM 1 .-. u)) `newEq` partialTime u
  -- Second equation
  (diffV .*. laplacianV .+. u .*. (v .**. realConstantM 2) .-. f .*. v .*.  (realConstantM 1 .+. k)) `newEq` partialTime u

