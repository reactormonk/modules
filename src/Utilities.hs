{-# LANGUAGE PolyKinds #-}
module Utilities where

import Universum
import V2
import Generics.SOP as SOP
import Generics.SOP.NP

pureRecipe :: Applicative effect => target -> Recipe effect target '[]
pureRecipe target = Recipe $ \_ -> pure target
