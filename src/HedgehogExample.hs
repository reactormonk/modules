{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module HedgehogExample where

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Universum
import V2
import Generics.SOP as SOP
import Utilities
import Functions

newtype Name = Name Text
  deriving stock (Eq, Show, Universum.Generic)
  deriving anyclass SOP.Generic

newtype Email = Email Text
  deriving stock (Eq, Show, Universum.Generic)
  deriving anyclass SOP.Generic

data Person = Person
  { _name :: Name
  , _email :: Email
  }
  deriving stock (Eq, Show, Universum.Generic)
  deriving anyclass SOP.Generic

data Company = Company
  { _employees :: [Person]
  }
  deriving stock (Eq, Show, Universum.Generic)
  deriving anyclass SOP.Generic

instance MonadGen m => DefaultRecipe Identity (m Name) where
  def = pureRecipe $ Name <$> text (linear 3 20) unicode

instance MonadGen m => DefaultRecipe Identity (m Email) where
  def = pureRecipe $ do
    user <- text (linear 3 20) ascii
    host <- text (linear 3 10) ascii
    pure $ Email $ (user <> "@" <> host)

Utilities.genericTransientRecipeInstance ''Person

genCompany :: MonadGen m => (m Person) -> (m Company)
genCompany genP = do
  employees <- Gen.list (linear 3 10) genP
  pure $ Company employees

-- gFun :: (m Company)
-- gFun genP = undefined

-- gFun2 :: Company
-- gFun2 genP = undefined

instance MonadGen m => DefaultRecipe Identity (m Company) where
  type DefaultRecipeDeps Identity (m Company) = '[m Person]
  def = npToTRecipe $ uncurryNP (genCompany @m)

-- Functions.genericTransientRecipeInstance 'gFun2

-- Functions.genericTransientRecipeInstance 'gFun

-- Functions.genericTransientRecipeInstance 'genCompany

regularGen :: MonadGen m => m Company
regularGen = finishPure ()

largeCompanyGen' :: forall (m :: * -> *). MonadGen m => Recipe Identity (m Company) '[m Person]
largeCompanyGen' = Recipe $ \deps -> pure $ do
  employees <- Gen.list (linear 100 1000) (getTyped deps)
  pure $ Company employees

largeCompanyGen :: forall m. MonadGen m => (m Company)
largeCompanyGen = finishPure (I $ largeCompanyGen' @m) -- TODO why is this annotation required?

fixedEmailGen' :: forall m. MonadGen m => Text -> Recipe Identity (m Email) '[]
fixedEmailGen' domain = pureRecipe $ do
  user <- text (linear 3 20) ascii
  pure $ Email $ (user <> "@" <> domain)

fixedEmailGen :: forall m. MonadGen m => (m Company)
fixedEmailGen = finishPure (I $ fixedEmailGen' @m "company.com")

bothOverrides :: forall m. MonadGen m => (m Company)
bothOverrides = finishPure (fixedEmailGen' @m "company.com", largeCompanyGen' @m)
