module Main where

import Prelude 

import Effect (Effect)
import Effect.Console (log)
import GLPK
{-
-- a spacy concact
(+_+) :: String -> String -> String
x (+_+) y = x <> " " <> " " <> y 
-- a newliney concact
(+n+) :: String -> String -> String
x (+n+) y = x <> "\n" <> y 
-- a plussy concact
(+++) :: String -> String -> String
x (+++) y = x +_+ "+" +_+ y 

data Var = Var String
data LinExpr = LinExpr [(Number, Var)] -- Ax
data AffineExpr = AffineExpr LinExpr Number -- Ax + b
data Constraint = Contraint AffineExpr Ordering AffineExpr -- Ax + b <= Bx + c
data Objective = Min LinExpr | Max LinExpr
data Problem = Problem Objective [Constraint]


(<=)
===
<=


instance semigroupAffineExpr :: Semigroup AffineExpr where
  append (LinExpr x) (LinExpr y) = LinExpr (x <> y)




printProblem ::
printProblem (Problem obj constraints) =       where
												= printObjective obj

printVar :: Var -> String
printVar (Var s) = s

printVars :: [Var] -> String
printVars (Var s):vars = s <> "\n"
prinvVars (Var s):[] = s

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