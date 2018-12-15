module Main where

import Prelude 

import Effect (Effect)
import Effect.Console (log)
import Data.List
import Data.Generic.Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Foldable
import GLPK
import Foreign.Object as FO
import Data.Maybe

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
type Term = {coeff :: Number, var :: Var}
data LinExpr = LinExpr (List Term)  -- Ax
data AffineExpr = AffineExpr LinExpr Number -- Ax + b
data Constraint = Constraint AffineExpr Ordering AffineExpr -- Ax + b <= Bx + c
data Objective = Min LinExpr | Max LinExpr
data Problem = Problem Objective (List Constraint)


derive instance genericVar :: Generic Var _
derive instance genericLinExpr :: Generic LinExpr _
derive instance genericAffineExpr :: Generic AffineExpr _
derive instance genericConstraint :: Generic Constraint _
derive instance genericCObjective :: Generic Objective _
derive instance genericProblem :: Generic Problem _

instance showVar :: Show Var where
  show x = genericShow x
instance showLinExpr :: Show LinExpr where
  show x = genericShow x
instance showAffineExpr :: Show AffineExpr where
  show x = genericShow x
instance showConstraint :: Show Constraint where
  show x = genericShow x
instance showObjective :: Show Objective where
  show x = genericShow x
instance showProblem :: Show Problem where
  show x = genericShow x

cmpConstraint :: Ordering -> AffineExpr -> AffineExpr -> Constraint
cmpConstraint w x y = Constraint x w y
lte :: AffineExpr -> AffineExpr -> Constraint
lte = cmpConstraint LT
gte :: AffineExpr -> AffineExpr -> Constraint
gte = cmpConstraint GT
eq :: AffineExpr -> AffineExpr -> Constraint
eq = cmpConstraint EQ

infixl 4 lte as :<=
infixl 4 gte as :>=
infixl 4 eq as :==

--smul :: Number -> Var -> AffineExpr
--smul x v = AffineExpr (LinExpr ({coeff : x, var : v} : Nil)) 0.0



smul :: Number -> AffineExpr -> AffineExpr
smul n (AffineExpr (LinExpr ls) b) = AffineExpr (LinExpr (map (termmul n) ls)) (b*n) where
     termmul n' {coeff : c, var : v} = {coeff : c * n', var : v }
infixl 6 smul as :*


var :: String -> AffineExpr
var s = AffineExpr (LinExpr ({coeff : 1.0, var : (Var s)} : Nil)) 0.0



dropConstant :: AffineExpr -> LinExpr
dropConstant (AffineExpr x b) = x


asum :: forall f. Foldable f => f AffineExpr -> AffineExpr
asum = foldl (:+) azero


aadd :: AffineExpr -> AffineExpr -> AffineExpr
aadd (AffineExpr (LinExpr x) b) (AffineExpr (LinExpr y) c) = AffineExpr (LinExpr (x <> y)) (b + c) 
infixl 6 aadd as :+

apure :: Number -> AffineExpr
apure n = (AffineExpr (LinExpr Nil) n)
azero = apure 0.0
aone = apure 1.0 
{-
instance semiringAffineExpr :: SemiRing AffineExpr where
  add (AffineExpr (LinExpr x) b) (AffineExpr (LinExpr y) c) = AffineExpr (LinExpr (x <> y) (b + c)) 
  zero = (AffineExpr Nil 0.0)
  mul =  (AffineExpr (LinExpr (({coeff : 0.0, var : Var "NO MULS YOU JACKASS"}) : Nil)) 0.0)  
  one = (AffineExpr Nil 1.0) 
  append (LinExpr x) (LinExpr y) = LinExpr (x <> y)
-}
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

example :: Constraint
example =  var "x1" :+ var "x2" :+ var "x3" :== var "x4"

xs = map (\n -> var ("x" <> show n)) (1 .. 10)
exampleProblem2 = Problem (Max $ dropConstant $ (asum xs)) (xpos <> xmax)  where
     xpos = map (\x -> x :>= azero) xs
     xmax = map (\x -> x :<= apure 3.0) xs




lookupVar :: FO.Object Number -> Var -> Maybe Number
lookupVar o (Var s) = FO.lookup s o 
{-
lookupVar' :: FO.Object Number -> AffineExpr -> Maybe Number
lookupVar' o (Var s) = FO.lookup s o  
-} 
main :: Effect Unit
main = do
  --lp <- glp_create_prob
  --pure unit
  log (printProblem exampleProblem2)
