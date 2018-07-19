{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module BenchData where

import V2
import Universum
import Data.Diverse.Many
import qualified Data.Generics.Product as P
import Utilities

newtype M0 = M0 ()
instance DefaultRecipe Identity M0 where
  type DefaultRecipeDeps Identity M0 = '[]
  def = pureRecipe $ M0 ()

newtype M1 = M1 M0
instance DefaultRecipe Identity M1 where
  type DefaultRecipeDeps Identity M1 = '[M0]
  def = Recipe $ \deps -> pure $ M1 (getTyped deps)

newtype M2 = M2 M1
instance DefaultRecipe Identity M2 where
  type DefaultRecipeDeps Identity M2 = '[M1]
  def = Recipe $ \deps -> pure $ M2 (getTyped deps)

newtype M3 = M3 M2
instance DefaultRecipe Identity M3 where
  type DefaultRecipeDeps Identity M3 = '[M2]
  def = Recipe $ \deps -> pure $ M3 (getTyped deps)

newtype M4 = M4 M3
instance DefaultRecipe Identity M4 where
  type DefaultRecipeDeps Identity M4 = '[M3]
  def = Recipe $ \deps -> pure $ M4 (getTyped deps)

newtype M5 = M5 M4
instance DefaultRecipe Identity M5 where
  type DefaultRecipeDeps Identity M5 = '[M4]
  def = Recipe $ \deps -> pure $ M5 (getTyped deps)

newtype M6 = M6 M5
instance DefaultRecipe Identity M6 where
  type DefaultRecipeDeps Identity M6 = '[M5]
  def = Recipe $ \deps -> pure $ M6 (getTyped deps)

newtype M7 = M7 M6
instance DefaultRecipe Identity M7 where
  type DefaultRecipeDeps Identity M7 = '[M6]
  def = Recipe $ \deps -> pure $ M7 (getTyped deps)

newtype M8 = M8 M7
instance DefaultRecipe Identity M8 where
  type DefaultRecipeDeps Identity M8 = '[M7]
  def = Recipe $ \deps -> pure $ M8 (getTyped deps)

newtype M9 = M9 M8
instance DefaultRecipe Identity M9 where
  type DefaultRecipeDeps Identity M9 = '[M8]
  def = Recipe $ \deps -> pure $ M9 (getTyped deps)

newtype M10 = M10 M9
instance DefaultRecipe Identity M10 where
  type DefaultRecipeDeps Identity M10 = '[M9]
  def = Recipe $ \deps -> pure $ M10 (getTyped deps)

newtype M11 = M11 M10
instance DefaultRecipe Identity M11 where
  type DefaultRecipeDeps Identity M11 = '[M10]
  def = Recipe $ \deps -> pure $ M11 (getTyped deps)

newtype M12 = M12 M11
instance DefaultRecipe Identity M12 where
  type DefaultRecipeDeps Identity M12 = '[M11]
  def = Recipe $ \deps -> pure $ M12 (getTyped deps)

newtype M13 = M13 M12
instance DefaultRecipe Identity M13 where
  type DefaultRecipeDeps Identity M13 = '[M12]
  def = Recipe $ \deps -> pure $ M13 (getTyped deps)

newtype M14 = M14 M13
instance DefaultRecipe Identity M14 where
  type DefaultRecipeDeps Identity M14 = '[M13]
  def = Recipe $ \deps -> pure $ M14 (getTyped deps)

newtype M15 = M15 M14
instance DefaultRecipe Identity M15 where
  type DefaultRecipeDeps Identity M15 = '[M14]
  def = Recipe $ \deps -> pure $ M15 (getTyped deps)

newtype M16 = M16 M15
instance DefaultRecipe Identity M16 where
  type DefaultRecipeDeps Identity M16 = '[M15]
  def = Recipe $ \deps -> pure $ M16 (getTyped deps)

newtype M17 = M17 M16
instance DefaultRecipe Identity M17 where
  type DefaultRecipeDeps Identity M17 = '[M16]
  def = Recipe $ \deps -> pure $ M17 (getTyped deps)

newtype M18 = M18 M17
instance DefaultRecipe Identity M18 where
  type DefaultRecipeDeps Identity M18 = '[M17]
  def = Recipe $ \deps -> pure $ M18 (getTyped deps)

newtype M19 = M19 M18
instance DefaultRecipe Identity M19 where
  type DefaultRecipeDeps Identity M19 = '[M18]
  def = Recipe $ \deps -> pure $ M19 (getTyped deps)

newtype M20 = M20 M19
instance DefaultRecipe Identity M20 where
  type DefaultRecipeDeps Identity M20 = '[M19]
  def = Recipe $ \deps -> pure $ M20 (getTyped deps)

newtype M21 = M21 M20
instance DefaultRecipe Identity M21 where
  type DefaultRecipeDeps Identity M21 = '[M20]
  def = Recipe $ \deps -> pure $ M21 (getTyped deps)

newtype M22 = M22 M21
instance DefaultRecipe Identity M22 where
  type DefaultRecipeDeps Identity M22 = '[M21]
  def = Recipe $ \deps -> pure $ M22 (getTyped deps)

newtype M23 = M23 M22
instance DefaultRecipe Identity M23 where
  type DefaultRecipeDeps Identity M23 = '[M22]
  def = Recipe $ \deps -> pure $ M23 (getTyped deps)

newtype M24 = M24 M23
instance DefaultRecipe Identity M24 where
  type DefaultRecipeDeps Identity M24 = '[M23]
  def = Recipe $ \deps -> pure $ M24 (getTyped deps)

newtype M25 = M25 M24
instance DefaultRecipe Identity M25 where
  type DefaultRecipeDeps Identity M25 = '[M24]
  def = Recipe $ \deps -> pure $ M25 (getTyped deps)

newtype M26 = M26 M25
instance DefaultRecipe Identity M26 where
  type DefaultRecipeDeps Identity M26 = '[M25]
  def = Recipe $ \deps -> pure $ M26 (getTyped deps)

newtype M27 = M27 M26
instance DefaultRecipe Identity M27 where
  type DefaultRecipeDeps Identity M27 = '[M26]
  def = Recipe $ \deps -> pure $ M27 (getTyped deps)

newtype M28 = M28 M27
instance DefaultRecipe Identity M28 where
  type DefaultRecipeDeps Identity M28 = '[M27]
  def = Recipe $ \deps -> pure $ M28 (getTyped deps)

newtype M29 = M29 M28
instance DefaultRecipe Identity M29 where
  type DefaultRecipeDeps Identity M29 = '[M28]
  def = Recipe $ \deps -> pure $ M29 (getTyped deps)

newtype M30 = M30 M29
instance DefaultRecipe Identity M30 where
  type DefaultRecipeDeps Identity M30 = '[M29]
  def = Recipe $ \deps -> pure $ M30 (getTyped deps)

newtype M31 = M31 M30
instance DefaultRecipe Identity M31 where
  type DefaultRecipeDeps Identity M31 = '[M30]
  def = Recipe $ \deps -> pure $ M31 (getTyped deps)

newtype M32 = M32 M31
instance DefaultRecipe Identity M32 where
  type DefaultRecipeDeps Identity M32 = '[M31]
  def = Recipe $ \deps -> pure $ M32 (getTyped deps)

newtype M33 = M33 M32
instance DefaultRecipe Identity M33 where
  type DefaultRecipeDeps Identity M33 = '[M32]
  def = Recipe $ \deps -> pure $ M33 (getTyped deps)

newtype M34 = M34 M33
instance DefaultRecipe Identity M34 where
  type DefaultRecipeDeps Identity M34 = '[M33]
  def = Recipe $ \deps -> pure $ M34 (getTyped deps)

newtype M35 = M35 M34
instance DefaultRecipe Identity M35 where
  type DefaultRecipeDeps Identity M35 = '[M34]
  def = Recipe $ \deps -> pure $ M35 (getTyped deps)

newtype M36 = M36 M35
instance DefaultRecipe Identity M36 where
  type DefaultRecipeDeps Identity M36 = '[M35]
  def = Recipe $ \deps -> pure $ M36 (getTyped deps)

newtype M37 = M37 M36
instance DefaultRecipe Identity M37 where
  type DefaultRecipeDeps Identity M37 = '[M36]
  def = Recipe $ \deps -> pure $ M37 (getTyped deps)

newtype M38 = M38 M37
instance DefaultRecipe Identity M38 where
  type DefaultRecipeDeps Identity M38 = '[M37]
  def = Recipe $ \deps -> pure $ M38 (getTyped deps)

newtype M39 = M39 M38
instance DefaultRecipe Identity M39 where
  type DefaultRecipeDeps Identity M39 = '[M38]
  def = Recipe $ \deps -> pure $ M39 (getTyped deps)

newtype M40 = M40 M39
instance DefaultRecipe Identity M40 where
  type DefaultRecipeDeps Identity M40 = '[M39]
  def = Recipe $ \deps -> pure $ M40 (getTyped deps)

newtype M41 = M41 M40
instance DefaultRecipe Identity M41 where
  type DefaultRecipeDeps Identity M41 = '[M40]
  def = Recipe $ \deps -> pure $ M41 (getTyped deps)

newtype M42 = M42 M41
instance DefaultRecipe Identity M42 where
  type DefaultRecipeDeps Identity M42 = '[M41]
  def = Recipe $ \deps -> pure $ M42 (getTyped deps)

newtype M43 = M43 M42
instance DefaultRecipe Identity M43 where
  type DefaultRecipeDeps Identity M43 = '[M42]
  def = Recipe $ \deps -> pure $ M43 (getTyped deps)

newtype M44 = M44 M43
instance DefaultRecipe Identity M44 where
  type DefaultRecipeDeps Identity M44 = '[M43]
  def = Recipe $ \deps -> pure $ M44 (getTyped deps)

newtype M45 = M45 M44
instance DefaultRecipe Identity M45 where
  type DefaultRecipeDeps Identity M45 = '[M44]
  def = Recipe $ \deps -> pure $ M45 (getTyped deps)

newtype M46 = M46 M45
instance DefaultRecipe Identity M46 where
  type DefaultRecipeDeps Identity M46 = '[M45]
  def = Recipe $ \deps -> pure $ M46 (getTyped deps)

newtype M47 = M47 M46
instance DefaultRecipe Identity M47 where
  type DefaultRecipeDeps Identity M47 = '[M46]
  def = Recipe $ \deps -> pure $ M47 (getTyped deps)

newtype M48 = M48 M47
instance DefaultRecipe Identity M48 where
  type DefaultRecipeDeps Identity M48 = '[M47]
  def = Recipe $ \deps -> pure $ M48 (getTyped deps)

newtype M49 = M49 M48
instance DefaultRecipe Identity M49 where
  type DefaultRecipeDeps Identity M49 = '[M48]
  def = Recipe $ \deps -> pure $ M49 (getTyped deps)
