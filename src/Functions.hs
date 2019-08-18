{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}

module Functions where

import V2
import Utilities
import Universum hiding (Type)
import Generics.SOP as SOP
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH hiding (Type)
import GHC.Types

genericTransientRecipeInstance :: TH.Name -> TH.DecsQ
genericTransientRecipeInstance fun = do
  VarI name funT _ <- reify fun
  let resultType = funToResult funT
  res <- [d|
    instance DefaultRecipe Identity $(pure resultType) where
      type DefaultRecipeDeps Identity $(pure resultType) = UncurryDeps $(pure funT)
      def = toTRecipe $(varE name)
    |]
  pure res

funToResult :: TH.Type -> TH.Type
funToResult (AppT (AppT ArrowT _) (AppT rec@(AppT ArrowT _) _)) = funToResult rec
funToResult (AppT (AppT ArrowT _) res) = res
funToResult res@(AppT _ _) = res
funToResult s = error (show s)

function :: Monad m => Int -> Int -> Int -> m Bool
function = undefined

-- fun :: Monad m => NP I '[Int, Int, Int] -> m Bool
-- fun = uncurryNP function

toTRecipe :: (Uncurry fun (UncurryDeps fun) (UncurryResult fun)) =>
  fun -> Recipe Identity (UncurryResult fun) (UncurryDeps fun)
toTRecipe = npToTRecipe . uncurryNP

npToTRecipe :: (NP I types -> r) -> Recipe Identity r types
npToTRecipe f = Recipe $ \deps -> Identity $ f (fromHasTypesToNP deps)

-- toCRecipe :: fun -> Recipe effect (UncurryResult fun) (UncurryDeps fun)
-- toCRecipe = npToCRecipe . uncurryNP

npToCRecipe :: (NP I types -> m r) -> Recipe m r types
npToCRecipe f = Recipe $ \deps -> f (fromHasTypesToNP deps)

type family UncurryDeps fun :: [Type] where
  UncurryDeps (t -> fun) = t ': UncurryDeps fun
  UncurryDeps t = '[]

type family UncurryResult fun :: Type where
  UncurryResult (t -> fun) = UncurryResult fun
  UncurryResult t = t

uncurryNP :: (Uncurry fun (UncurryDeps fun) (UncurryResult fun)) =>
  fun -> NP I (UncurryDeps fun) -> (UncurryResult fun)
uncurryNP = uncurryNP'

-- mostly provided by lyxia

class Uncurry fun types r where
  uncurryNP' :: fun -> NP I types -> r

instance forall fun r t types.
  ( Uncurry fun types r
  ) => Uncurry (t -> fun) (t ': types) r where
  uncurryNP' f ((I a) :* as) = uncurryNP' (f a) as

instance (s ~ r) => Uncurry s '[] r where
  uncurryNP' z Nil = z
