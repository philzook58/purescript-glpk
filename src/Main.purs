module Main where

import Prelude 

import Effect (Effect)
import Effect.Console (log)
import Data.Array
import GLPK

-- a spacy concact

spaceAppend :: String -> String -> String
spaceAppend x y = x <> " " <> " " <> y 

-- purescript has to define infix operators using a seperate syntax
-- +_+ is an "operator section" a weird purescipt feature
-- it appears operators can't have characters in them
infix 5 spaceAppend as +++
-- a newliney concact
newlineAppend :: String -> String -> String
newlineAppend x y = x <> "\n" <> y 

infix 5 newlineAppend as ++:

-- a plussy concact
{-
(+++) newlineAppend :: String -> String -> String
x (+++) y = x +_+ "+" +_+ y 
-}


data Var = Var String
data LinExpr = LinExpr (Array {coeff :: Number, var :: Var})  -- Ax
data AffineExpr = AffineExpr LinExpr Number -- Ax + b
data Constraint = Contraint AffineExpr Ordering AffineExpr -- Ax + b <= Bx + c
data Objective = Min LinExpr | Max LinExpr
data Problem = Problem Objective (Array Constraint)

{-
(:<=) :: AffineExpr -> AffineExpr -> Constraint
(:==) :: AffineExpr -> AffineExpr -> Constraint
(:<=) :: AffineExpr -> AffineExpr -> Constraint
(:+) :: AffineExpr -> AffineExpr -> AffineExpr
(:-) :: AffineExpr -> AffineExpr -> AffineExpr
(:*)  :: Number -> Var -> AffineExpr
-}

instance semigroupAffineExpr :: Semigroup LinExpr where
  append (LinExpr x) (LinExpr y) = LinExpr (x <> y)



{-
printProblem ::
printProblem (Problem obj constraints) =       where
												= printObjective obj
-}
printVar :: Var -> String
printVar (Var s) = s

{-
printVars :: [Var] -> String
printVars (Var s):vars = s <> "\n"
prinvVars (Var s):[] = s

-}

-- You can pattern match on Lists and Sequences, but not arrays
{-
printVars :: List Var -> String
printVars [] = mempty
printVars (v:vars) = (printVar v) <> "\n" <> (printVars vars)
-}
printVars :: Array Var -> String
printVars = foldMap (\v -> (printVar v) <> "\n") 


{-
printObjective :: Objective -> String
printObjective (Min expr) = "Minimize" +n+ "obj:" +_+ (printLinExpr expr)
printObjective (Max expr) = "Maximize" +n+ "obj:" +_+ (printLinExpr expr)

printLinExpr :: LinExpr -> String
printLinExpr (LinExpr ((c, Var var):terms)) = (show c) +_+ var +++ (printLinExpr terms)
printLinExpr (LinExpr []) = ""

printAffineExpr :: AffineExpr -> String
printAffineExpr (AffineExpr linExpr const) = (printLinExpr linExpr) +++ const

printConstraint :: Constraint -> String
printConstraint (Constraint affineExpr1 cmp affineExpr2) =  (printAffineExpr affineExpr1) +_+ (show cmp) +_+ (printAffineExpr affineExpr2)
-}



main :: Effect Unit
main = do
  log "Hello sailor!"
  lp <- glp_create_prob
  pure unit