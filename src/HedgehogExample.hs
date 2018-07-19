{-# LANGUAGE ApplicativeDo #-}
module HedgehogExample where

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Universum
import V2
import Generics.SOP
import Utilities

newtype Name = Name Text
newtype Email = Email Text

data Person = Person
  { _name :: Name
  , _email :: Email
  }

data Company = Company
  { _employees :: [Person]
  }

instance MonadGen m => DefaultRecipe Identity (m Name) where
  type DefaultRecipeDeps Identity (m Name) = '[]
  def = pureRecipe $ Name <$> text (linear 3 20) unicode

instance MonadGen m => DefaultRecipe Identity (m Email) where
  type DefaultRecipeDeps Identity (m Email) = '[]
  def = pureRecipe $ do
    user <- text (linear 3 20) ascii
    host <- text (linear 3 10) ascii
    pure $ Email $ (user <> "@" <> host)

instance (Applicative m, Applicative n) => DefaultRecipe m (n Person) where
  type DefaultRecipeDeps m (n Person) = '[n Name, n Email]
  def = Recipe $ \deps -> pure $ do
    name <- getTyped deps
    email <- getTyped deps
    pure $ Person name email

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
