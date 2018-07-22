{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module HedgehogExample where

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Universum
import V2
import Generics.SOP as SOP
import Utilities
import Generics.SOP.Constraint

newtype Name = Name Text deriving (Eq, Show, Universum.Generic, SOP.Generic)
newtype Email = Email Text deriving (Eq, Show, Universum.Generic, SOP.Generic)

data Person = Person
  { _name :: Name
  , _email :: Email
  } deriving (Eq, Show, Universum.Generic, SOP.Generic)

data Company = Company
  { _employees :: [Person]
  } deriving (Eq, Show, Universum.Generic, SOP.Generic)

instance MonadGen m => DefaultRecipe Identity (m Name) where
  def = pureRecipe $ Name <$> text (linear 3 20) unicode

instance MonadGen m => DefaultRecipe Identity (m Email) where
  def = pureRecipe $ do
    user <- text (linear 3 20) ascii
    host <- text (linear 3 10) ascii
    pure $ Email $ (user <> "@" <> host)

-- TODO be able to derive the function via TH
instance (Applicative m, Applicative n) => DefaultRecipe m (n Person) where
  type DefaultRecipeDeps m (n Person) = Lift n (Head (Code Person))
  def = genericTransientRecipe

instance MonadGen m => DefaultRecipe Identity (m Company) where
  type DefaultRecipeDeps Identity (m Company) = '[m Person]
  def = Recipe $ \deps -> pure $ do
    employees <- Gen.list (linear 3 10) (getTyped deps)
    pure $ Company employees

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
