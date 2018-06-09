{-# OPTIONS_GHC -freduction-depth=1000 #-}
module Benching where

import V2
-- import Universum
import Data.Diverse.Many
import BenchData

import Prelude
import Data.Proxy
import Data.Kind
import Data.Functor.Identity

c :: M20
c = runIdentity $ finish nil

-- c49 :: M49
-- c49 = runIdentity $ finish nil
