{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Modules where

import Universum
import V2
import Generics.SOP as SOP
import Utilities

newtype Seed = Seed Text

data SeedModule = SeedModule
  { genSeed :: IO Seed
  }

data StoreSeedModule = StoreSeedModule
  { storeSeed :: Seed -> IO ()
  , getSeed :: IO Seed
  }

data DisplayModule = DisplayModule
  { messageWithSeedFromStore :: IO Text
  }

data NetworkModule = NetworkModule
  { sendSeedOverWire :: IO ()
  }

data HttpModule = HttpModule
  { get :: Request -> IO Response
  }
