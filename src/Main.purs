module Main where

import Prelude 

import Effect (Effect)
import Effect.Console (log)
import Data.List
--import GLPK

-- a spacy concact

--spaceAppend :: String -> String -> String
--spaceAppend x y = x <> " " <> " " <> y 

-- purescript has to define infix operators using a seperate syntax
-- +_+ is an "operator section" a weird purescipt feature
-- it appears operators can't have characters in them
infixr 5 spaceAppend as +$+
-- a spacey concact
spaceAppend :: String -> String -> String
spaceAppend x y = x <> " " <> y 

infixr 5 newlineAppend as +\+
-- a newliney concact
newlineAppend :: String -> String -> String
newlineAppend x y = x <> "\n" <> y 


infixr 5 plusAppend as +++
-- a newliney concact
plusAppend :: String -> String -> String
plusAppend x y = x +$+ "+" +$+ y 

-- a plussy concact
{-
(+++) newlineAppend :: String -> String -> String
x (+++) y = x +_+ "+" +_+ y 
-}


data Var = Var String
data LinExpr = LinExpr (List {coeff :: Number, var :: Var})  -- Ax
data AffineExpr = AffineExpr LinExpr Number -- Ax + b
data Constraint = Constraint AffineExpr Ordering AffineExpr -- Ax + b <= Bx + c
data Objective = Min LinExpr | Max LinExpr
data Problem = Problem Objective (List Constraint)

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

printVar :: Var -> String
printVar (Var s) = s

printVars :: List Var -> String
printVars = foldl (\acc v -> acc +\+ (printVar v)) mempty

printTerm :: {coeff :: Number, var :: Var} -> String
printTerm {coeff, var} = (show coeff) +$+ (printVar var)

printComp :: Ordering -> String
printComp LT = "<="
printComp GT = ">="
printComp EQ = "=="

printObjective :: Objective -> String
printObjective (Min expr) = "Minimize" +\+ "obj:" +$+ (printLinExpr expr)
printObjective (Max expr) = "Maximize" +\+ "obj:" +$+ (printLinExpr expr)

printLinExpr :: LinExpr -> String
printLinExpr (LinExpr terms) = foldl (\acc t -> acc +++ (printTerm t)) mempty terms

printAffineExpr :: AffineExpr -> String
printAffineExpr (AffineExpr linExpr const) = (printLinExpr linExpr) +++ (show const)

printConstraint :: Constraint -> String
printConstraint (Constraint affineExpr1 cmp affineExpr2) =  (printAffineExpr affineExpr1) +$+ (printComp cmp) +$+ (printAffineExpr affineExpr2)

printConstraints :: List Constraint -> String
printConstraints cons = "Subject To" <> (foldl (\acc c -> acc +\+ (printConstraint c)) mempty cons)

printProblem :: Problem -> String
printProblem (Problem obj constraints) = (printObjective obj) +\+ (printConstraints constraints) +\+ "End"

exampleProblem = (Problem 
  (Min (LinExpr ({coeff:5.0, var:(Var "v1")}:{coeff:6.0, var:(Var "v2")}:Nil))) 
  ((Constraint (AffineExpr (LinExpr Nil) 1.0) (GT) (AffineExpr (LinExpr Nil) 1.0)):Nil)
)

main :: Effect Unit
main = do
  --lp <- glp_create_prob
  --pure unit
  log (printProblem exampleProblem)
