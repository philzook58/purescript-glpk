module Main where

import Prelude 

import Effect (Effect)
import Effect.Console (log)
import Data.List
import Data.Generic.Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Foldable
import GLPK
import Foreign.Object as FO
import Data.Maybe
import Data.Int (fromString)
import Data.Map as M
import Vector
import Data.String as S
import Data.Array as A
import Data.Int as I
import Data.Tuple
import Data.Set as Set
import Data.FoldableWithIndex
import Data.Ord
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

type LinExpr k = M.Map k Number
data AffineExpr k = AffineExpr (LinExpr k) Number -- Ax + b. Should we 
data Constraint k = Constraint (AffineExpr k) Ordering (AffineExpr k) -- Ax + b <= Bx + c
data Objective k = Min (LinExpr k) | Max (LinExpr k)
data Problem k = Problem (Objective k) (List (Constraint k))

cmpConstraint :: forall k. Ordering -> AffineExpr k -> AffineExpr k -> Constraint k
cmpConstraint w x y = Constraint x w y
lte :: forall k. AffineExpr k -> AffineExpr k -> Constraint k
lte = cmpConstraint LT
gte :: forall k. AffineExpr k -> AffineExpr k -> Constraint k
gte = cmpConstraint GT
eqC :: forall k. AffineExpr k -> AffineExpr k -> Constraint k
eqC = cmpConstraint EQ

infixl 4 lte as :<=
infixl 4 gte as :>=
infixl 4 eqC as :==

--smul :: Number -> Var -> AffineExpr
--smul x v = AffineExpr (LinExpr ({coeff : x, var : v} : Nil)) 0.0



{-
smul :: Number -> AffineExpr -> AffineExpr
smul n (AffineExpr ls b) = AffineExpr ((map (n * _) ls)) (b*n) where
     termmul n' {coeff : c, var : v} = {coeff : c * n', var : v }
infixl 6 smul as :*
-}

var :: forall k. k -> AffineExpr k
var k = AffineExpr (M.singleton k 1.0) 0.0


dropConstant :: forall k. AffineExpr k -> LinExpr k
dropConstant (AffineExpr x b) = x

dropVars :: forall k. AffineExpr k -> Number
dropVars (AffineExpr x b) = b

asum :: forall f k. Foldable f => Ord k => f (AffineExpr k) -> AffineExpr k
asum = foldl (:+) azero

aminus :: forall k. Ord k => AffineExpr k -> AffineExpr k -> AffineExpr k
aminus (AffineExpr (x) b) (AffineExpr (y) c) = AffineExpr ((x ^-^ y)) (b + c) 

aadd :: forall k. Ord k => AffineExpr k -> AffineExpr k -> AffineExpr k
aadd (AffineExpr (x) b) (AffineExpr (y) c) = AffineExpr ((x ^+^ y)) (b + c) 
infixl 6 aadd as :+


apure :: forall k. Number -> AffineExpr k
apure n = (AffineExpr (M.empty) n)
azero :: forall k. AffineExpr k
azero = apure 0.0
aone :: forall k. AffineExpr k
aone = apure 1.0 

showComp :: Ordering -> String
showComp LT = "<="
showComp GT = ">="
showComp EQ = "=="

allVars :: forall k. Ord k => Problem k -> Set.Set k
allVars (Problem o cs) = Set.union (allVarsO o) (Set.unions $ map allVarsC cs) where
  allVarsO (Min l) = M.keys l
  allVarsO (Max l) = M.keys l
  allVarsC (Constraint l _ r) = Set.union (allVarsA l) (allVarsA r)
  -- allVarsA :: forall k. Ord k => AffineExpr k -> Set.Set k
  allVarsA (AffineExpr l _) = M.keys l

data VarMap k = VarMap (M.Map k Int) (M.Map Int k)

varMap :: forall k. Ord k => Problem k -> VarMap k
varMap prob = VarMap keymap indexmap where 
     set = allVars prob
     setarray = A.fromFoldable set
     indexmap = M.fromFoldable $ A.mapWithIndex (\i k -> Tuple i k) setarray
     keymap = M.fromFoldable $ A.mapWithIndex (\i k -> Tuple k i) setarray

showObjective :: forall k. Ord k => Objective k -> VarMap k -> String
showObjective (Min expr) m = "Minimize" +\+ "obj:" +$+ (showLinExpr expr m)
showObjective (Max expr) m = "Maximize" +\+ "obj:" +$+ (showLinExpr expr m)
{-
showLinExpr :: forall k. LinExpr k -> String
showLinExpr (terms) = foldl (\acc t -> acc +++ (showTerm t)) mempty terms
-}
showLinExpr :: forall k. Ord k => LinExpr k -> VarMap k -> String -- Reader (VarMap k) String
showLinExpr m (VarMap keymap indexmap) = foldMapWithIndex (\key n -> " + " <> show n <> " " <> "x" <> (show $ fromMaybe 0 (M.lookup key keymap))) m

{-

-}

readVars :: forall k. Ord k => FO.Object Number -> VarMap k -> M.Map k Number
readVars fo (VarMap keymap indexmap) = M.fromFoldable $ mapMaybe string2Key $ FO.toUnfoldable fo where
                       string2Key :: Tuple String Number -> Maybe (Tuple k Number)
                       string2Key (Tuple s n) = do 
                                      x <- S.uncons s
                                      i <- I.fromString x.tail
                                      k <- M.lookup i indexmap
                                      pure $ Tuple k n

                     --  fromMaybe 0 $ I.fromString (maybe "0" (\x -> x.tail) (S.uncons s))


{-
lookupVar :: FO.Object Number -> Var -> Maybe Number
lookupVar o (Var s) = FO.lookup s o 
-}
showAffineExpr :: forall k. Ord k => AffineExpr k -> VarMap k -> String
showAffineExpr (AffineExpr linExpr const) m= (showLinExpr linExpr m) +++ (show const)

showConstraint :: forall k. Ord k => Constraint k -> VarMap k -> String
showConstraint (Constraint affineExpr1 cmp affineExpr2) m =  (showLinExpr lhs m) +$+ (showComp cmp) +$+ rhs where
   lhs = dropConstant $ aminus affineExpr1 affineExpr2 
   rhs = show $ (dropVars affineExpr2) - (dropVars affineExpr1)

showConstraints :: forall k. Ord k => List (Constraint k) -> VarMap k -> String
showConstraints cons m = "Subject To" <> (foldl (\acc c -> acc +\+ (showConstraint c m)) mempty cons)

showProblem :: forall k. Ord k => Problem k -> String
showProblem p@(Problem obj constraints) = (showObjective obj m) +\+ (showConstraints constraints m) +\+ "End" where
    m = varMap p
{-
exampleProblem = (Problem 
  (Min (({coeff:5.0, var:(Var "v1")}:{coeff:6.0, var:(Var "v2")}:Nil))) 
  ((Constraint (AffineExpr (Nil) 1.0) (GT) (AffineExpr (Nil) 1.0)):Nil)
)
-}

example :: Constraint Int
example =  var 1 :+ var 2 :+ var 3 :== var 4

{-
Affineable Int where
   affine x = apure (fromInt x)
-}

exampleProblem2 :: Problem Int
exampleProblem2 = Problem (Max $ dropConstant $ (asum xs)) (xpos <> xmax)  where
     xpos = map (\x -> x :>= azero) xs
     xmax = map (\x -> x :<= apure 3.0) xs
     xs = map var (1 .. 10)

{- -}
rangeVar :: Int -> List (AffineExpr Int) 
rangeVar n = map var (1 .. n)

data PathVar = PathVar Int Int
derive instance genericPathVar :: Generic PathVar _
instance eqVar :: Eq PathVar where
  eq x y = genericEq x y
instance ordVar :: Ord PathVar where
  compare x y = genericCompare x y
{-
-- data PathVars = Path (List Var) PathVars | Empty
type PathVars = List (List Var)

buildPathVars :: Int -> PathVars 
buildPathVars 0 = Nil
buildPathVars n = (rangeVar ("x" <> "_" <> show n <> "_") n) : buildPathVars (n - 1)
-}
{-
do 
 i <- (1 .. n)
-}

-- pathvar i j = var $ "x" <> "_" <> show i <> "_" <> show j
pathvar :: Int -> Int -> AffineExpr PathVar
pathvar i j = var (PathVar i j)
allouts :: Int -> Int -> List (AffineExpr PathVar)
allouts n i = map (pathvar i) (1 .. n)

twoedges :: Int -> List (Constraint PathVar)
twoedges n = do
            j <- 1 .. n
            pure ((asum $ allouts n j) :== apure 2.0)
-- map (\j -> (asum $ allouts n j) :== (apure 2.0)) (1 .. n)
-- don't use self path

diagszero :: Int -> List (Constraint PathVar)
diagszero n = map (\j -> (pathvar j j) :== (apure 0.0)) (1 .. n)

symmetry :: Int -> List (Constraint PathVar)
symmetry n = do
             i <- 1 .. n
             j <- 1 .. (i-1)
             pure $ pathvar i j :== pathvar j i


simpleObjective :: Int -> M.Map PathVar Number
simpleObjective n = M.fromFoldable $ do
                                      i <- 1 .. n
                                      j <- 1 .. n
                                      pure $ Tuple (PathVar i j) (I.toNumber $ abs $ i - j)

simpleSalesman :: Int -> Problem PathVar
simpleSalesman n = Problem (Min (simpleObjective n)) constraints where
                        constraints = (twoedges n) <> (diagszero n) <> (symmetry n)
--foldMap (\i -> map (\j -> pathvar i j :== pathvar j i) (1 .. (i-1))) (1 .. n)

-- subloopConstraint :: Int -> List Int -> Constraint
-- subloopConstraint n vs = asum (map (allouts n) vs) :== (List.length vs + 1)

-- gets the two edges attached to node 0

--getPath :: M.Map PathVar Number -> List Int

neighbor :: M.Map PathVar Number -> M.Map Int (Tuple Int Int)
neighbor m = k3 where
                x = M.filter (_ > 0.5) m
                x1 = fromFoldable $ Set.map (\(PathVar i j) -> Tuple i (Tuple j j)) $ M.keys x
                k1 = M.fromFoldable x1
                k2 = M.fromFoldable (reverse x1) -- first occurence takes precendence
                k3 = M.unionWith (\(Tuple i _) (Tuple j _) -> Tuple i j) k1 k2



followloop origin prev (Tuple a b) |  a == origin || b == origin = Nothing
                                   |  a == prev = Just b
                                   |  b == prev = Just a
                                   | otherwise = Nothing
      if a == origin || b == origin
                               Tuple a b = lookup v m

{-
keys $ filter (_ != 0) $ map (lookup k) (allouts n 0)


fstind = fromMaybe 0 do
			      x <- (split (Pattern "_") s !! 1)
			      fromString x
sndind = fromMaybe 0 do
			      x <- (split (Pattern "_") s !! 2)
			      fromString x
-}
-- maybe a fresh var monad
-- keep variables in Map (Int,Int)




-- maybe we should marshal from our type to 
-- (Ord k) -> Map k Int

{-

allVars :: Problem -> Set Vars
type Edges = Map Vertex Vertex
type Vertex = Int
type Path = List Vertex

-- no this doesn't work
followPath start prev current k = followPath lookup current k 
-}







{-
atMostN :: forall f. Foldable f => Int -> f Vars -> Constraint
atMostN n x = 


exactlyN :: forall f. Foldable f => Int -> f Vars -> Constraint
-}

{-
 	j <- (1 .. n)
 	let v = "x" <> "_" <> show i <> "_" <> show j
-}


{-
lookupVar' :: FO.Object Number -> AffineExpr -> Maybe Number
lookupVar' o (Var s) = FO.lookup s o  
-} 
main :: Effect Unit
main = do
  --lp <- glp_create_prob
  --pure unit
  log (showProblem exampleProblem2)
  res <- pure (glpk_solve_lp $ showProblem exampleProblem2)
  let (resmap :: M.Map Int Number) = readVars res.vars (varMap exampleProblem2)
  log (show res.obj)
  log (show res)
  log (show resmap)
  pure unit



{-
instance additiveAffine :: Ord k => Additive (AffineExpr k) where
  zerov = AffineExpr M.empty zero
  vadd (AffineExpr l x)  (AffineExpr l' y) = AffineExpr (vadd l l') (x + y)
  vdiff (AffineExpr l x)  (AffineExpr l' y) = AffineExpr (vdiff l l') (x - y)
-}
-- Do I want to ise lists of variables. 
-- Maps require Ord 

{-
-- derive instance genericVar :: Generic Var _
-- derive instance genericLinExpr :: Generic LinExpr _
derive instance genericAffineExpr :: Generic (AffineExpr k) _
derive instance genericConstraint :: Generic (Constraint k) _
derive instance genericCObjective :: Generic (Objective k) _
derive instance genericProblem :: Generic (Problem k) _
-}
--instance showVar :: Show Var where
--  show x = genericShow x
{-
instance eqVar :: Eq Var where
  eq x y = genericEq x y
instance ordVar :: Ord Var where
  compare x y = genericCompare x y
-}
--instance showLinExpr :: Show LinExpr where
--  show x = genericShow x
{-
instance showAffineExpr :: Show k => Show (AffineExpr k) where
  show x = genericShow x
instance showConstraint :: Show k => Show (Constraint k) where
  show x = genericShow x
instance showObjective :: Show k =>  Show (Objective k) where
  show x = genericShow x
instance showProblem :: Show k => Show (Problem k) where
  show x = genericShow x
-}

{-
instance semiringAffineExpr :: SemiRing AffineExpr where
  add (AffineExpr (LinExpr x) b) (AffineExpr (LinExpr y) c) = AffineExpr (LinExpr (x <> y) (b + c)) 
  zero = (AffineExpr Nil 0.0)
  mul =  (AffineExpr (LinExpr (({coeff : 0.0, var : Var "NO MULS YOU JACKASS"}) : Nil)) 0.0)  
  one = (AffineExpr Nil 1.0) 
  append (LinExpr x) (LinExpr y) = LinExpr (x <> y)
-}

{-
instance semigroupAffineExpr :: Semigroup LinExpr where
  append ( x) ( y) = (x <> y)
-}
{-
showVar :: Var -> String
showVar (Var s) = s

showVars :: List Var -> String
showVars = foldl (\acc v -> acc +\+ (showVar v)) mempty
-}
{-
showTerm :: {coeff :: Number, var :: Var} -> String
showTerm {coeff, var} = (show coeff) +$+ (showVar var)
-}