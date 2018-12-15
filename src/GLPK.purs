-- src/Math.purs
module GLPK where
import Effect (Effect)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Foreign.Object as FO
-- import Data.StrMap
-- foreign import pow :: Number -> Number -> Number

-- purescript-foreign-object

-- foreign import glp_create_prob :: Effect LPProb
-- foreign import glp_read_lp_from_string :: Fn2 LPProb String 
-- foreign import data LPProb :: Type
--foreign import data Result :: Type
foreign import glpk_solve_lp :: String -> {res :: FO.Object Number, obj :: Number}
-- foreign import lookupVar :: Fn2 String Result Number

-- lookupVar (Var s) o = FO.lookup s o  


