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

genName :: Gen Name
genName = Name <$> text (linear 3 20) unicode

Functions.genericTransientRecipeInstance 'genName

genEmail :: Gen Email
genEmail = do
  user <- text (linear 3 20) ascii
  host <- text (linear 3 10) ascii
  pure $ Email $ (user <> "@" <> host)

Functions.genericTransientRecipeInstance 'genEmail

Utilities.genericTransientRecipeInstance ''Person

genCompany :: Gen Person -> Gen Company
genCompany genP = do
  employees <- Gen.list (linear 3 10) genP
  pure $ Company employees

Functions.genericTransientRecipeInstance 'genCompany

regularGen :: Gen Company
regularGen = finishPure ()

largeCompanyGen' :: Recipe Identity (Gen Company) '[Gen Person]
largeCompanyGen' = Recipe $ \deps -> pure $ do
  employees <- Gen.list (linear 100 1000) (getTyped deps)
  pure $ Company employees

largeCompanyGen :: (Gen Company)
largeCompanyGen = finishPure (I $ largeCompanyGen') -- TODO why is this annotation required?

fixedEmailGen' :: Text -> Recipe Identity (Gen Email) '[]
fixedEmailGen' domain = pureRecipe $ do
  user <- text (linear 3 20) ascii
  pure $ Email $ (user <> "@" <> domain)

fixedEmailGen :: Gen Company
fixedEmailGen = finishPure (I $ fixedEmailGen' "company.com")

bothOverrides :: Gen Company
bothOverrides = finishPure (fixedEmailGen' "company.com", largeCompanyGen')
