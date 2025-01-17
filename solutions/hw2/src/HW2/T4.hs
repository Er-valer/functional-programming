{-# LANGUAGE InstanceSigs #-}
module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) :: ListPlus a -> ListPlus a -> ListPlus a
  (Last a) <> b  = a :+ b
  (x :+ xs) <> b = x :+ (xs <> b)

data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) :: Inclusive a b -> Inclusive a b -> Inclusive a b
  This x1 <> This x2       = This (x1 <> x2)
  This x1 <> That y2       = Both x1 y2
  This x1 <> Both x2 y2    = Both (x1 <> x2) y2

  That y1 <> This x2       = Both x2 y1
  That y1 <> That y2       = That (y1 <> y2)
  That y1 <> Both x2 y2    = Both x2 (y1 <> y2)

  Both x1 y1 <> This x2    = Both (x1 <> x2) y1
  Both x1 y1 <> That y2    = Both x1 (y1 <> y2)
  Both x1 y1 <> Both x2 y2 = Both (x1 <> x2) (y1 <> y2)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  (<>) :: DotString -> DotString -> DotString
  dsx <> (DS "")   = dsx
  (DS "") <> dsy   = dsy
  (DS x) <> (DS y) = DS (x ++ "." ++ y)

instance Monoid DotString where
  mempty :: DotString
  mempty = DS ""

newtype Fun a = F (a -> a)

-- (F f <> F g) <> F h == F ((f . g) . h) == F (f . (g . h)) == F f <> (F g <> F h)
instance Semigroup (Fun a) where
  (<>) :: Fun a -> Fun a -> Fun a
  F f <> F g = F (f . g)

-- F f <> F id == F (f . id) == F f
-- F id <> F f == F (id . f) == F f
instance Monoid (Fun a) where
  mempty :: Fun a
  mempty = F id
