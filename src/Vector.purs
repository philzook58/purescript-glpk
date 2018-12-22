module Vector where
import Data.Map
import Prelude
import Data.List
import Data.FoldableWithIndex


class Functor f <= Additive f where
  zerov :: forall a. Semiring a => f a
  vadd :: forall a. Semiring a => f a -> f a -> f a
  vdiff :: forall a. Ring a => f a -> f a -> f a


smul :: forall a f. Semiring a => Functor f => a -> f a -> f a 
smul x = map (x * _)


instance addMap :: Ord k => Additive (Map k) where
  zerov = empty
  vadd = unionWith (+)
  vdiff = unionWith (-)

--showLinExpr (LinExpr terms) = foldl (\acc t -> acc +++ (showTerm t)) mempty terms
showMapVect :: forall k a. Show k => Show a => Map k a -> String
showMapVect = foldlWithIndex (\k acc t -> acc <> " " <> (show t  <> " " <> show k)) mempty

{-
type Term k = Map k Number
data Constraint k = Constraint (Term k) Number
type Objective k = Term k
data Problem = Problem (Objective k) (List (Constraint k))
-}


infixl 6 vadd as ^+^
infixl 6 vdiff as ^-^
infixl 6 smul as ^*

--- insert
-- lookup 
-- singleton




