{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Functions where

import V2
import Utilities
import Universum hiding (Type)
import Generics.SOP as SOP
import Language.Haskell.TH

genericTransientRecipeInstance :: Name -> DecsQ
genericTransientRecipeInstance fun = do
  VarI _ funType _ <- reify fun
  -- instanceType :: Type
  -- let instanceType = undefined
  case funType of
    ForallT typeVars context typ -> do
      -- InstanceD Nothing [] instanceType
      let res = [d|
            instance Monad m => DefaultRecipe Identity (UncurryResult $(pure typ)) where
              type DefaultRecipeDeps Identity (UncurryResult $(pure typ)) = UncurryDeps $(pure typ)
              def = npToTRecipe $ uncurryNP ($(varE fun))
            |]
      res >>= print
      res
    ConT typ -> undefined

function :: Monad m => Int -> Int -> Int -> m Bool
function = undefined

fun :: Monad m => NP I '[Int, Int, Int] -> m Bool
fun = uncurryNP function

npToTRecipe :: (NP I types -> r) -> Recipe Identity r types
npToTRecipe f = Recipe $ \deps -> Identity $ f (fromHasTypesToNP deps)

npToCRecipe :: (NP I types -> m r) -> Recipe m r types
npToCRecipe f = Recipe $ \deps -> f (fromHasTypesToNP deps)

type family UncurryDeps fun :: [*] where
  UncurryDeps (t -> fun) = t ': UncurryDeps fun
  UncurryDeps t = '[]

type family UncurryResult fun :: * where
  UncurryResult (t -> fun) = UncurryResult fun
  UncurryResult t = t

-- mostly provided by lyxia

class Uncurry fun types r where
  uncurryNP :: fun -> NP I types -> r

instance forall fun r t types.
  ( Uncurry fun types r
  ) => Uncurry (t -> fun) (t ': types) r where
  uncurryNP f ((I a) :* as) = uncurryNP (f a) as

instance (s ~ r) => Uncurry s '[] r where
  uncurryNP z Nil = z
