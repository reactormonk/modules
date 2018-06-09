{-# LANGUAGE ApplicativeDo #-}
module HedgehogExample where

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Universum
import V2
import Data.Diverse

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

instance MonadGen m => DefaultRecipe Identity (m Person) where
  type DefaultRecipeDeps Identity (m Person) = '[m Name, m Email]
  def = Recipe $ \deps -> pure $ do
    name <- grab deps
    email <- grab deps
    pure $ Person name email

instance MonadGen m => DefaultRecipe Identity (m Company) where
  type DefaultRecipeDeps Identity (m Company) = '[m Person]
  def = Recipe $ \deps -> pure $ do
    employees <- Gen.list (linear 3 10) (grab deps)
    pure $ Company employees

regularGen :: MonadGen m => m Company
regularGen = runIdentity $ finish nil

largeCompanyGen' :: forall (m :: * -> *). MonadGen m => Recipe Identity (m Company) '[m Person]
largeCompanyGen' = Recipe $ \deps -> pure $ do
  employees <- Gen.list (linear 100 1000) (grab deps)
  pure $ Company employees

largeCompanyGen :: forall m. MonadGen m => (m Company)
largeCompanyGen = runIdentity $ finish (largeCompanyGen' @m ./ nil) -- TODO why is this annotation required?

fixedEmailGen' :: forall m. MonadGen m => Text -> Recipe Identity (m Email) '[]
fixedEmailGen' domain = pureRecipe $ do
  user <- text (linear 3 20) ascii
  pure $ Email $ (user <> "@" <> domain)

fixedEmailGen :: forall m. MonadGen m => (m Company)
fixedEmailGen = runIdentity $ finish (fixedEmailGen' @m "company.com" ./ nil)
