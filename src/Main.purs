module Main where

import Prelude 

import Effect (Effect)
import Effect.Console (log)
import GLPK
{-
-- a spacy concact
(+++) :: String -> String -> String
x (+++) y = x <> " " <> " " <> y 

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
printConstraint :: Constraint -> String
printConstraint cmp =  +++ (show cmp) +++
printAffine :: AffineExpr -> String
printAffine 
printVar :: Var -> String
printVar (Var s) = s
-}


main :: Effect Unit
main = do
  log "Hello sailor!"
  lp <- glp_create_prob
  pure unit