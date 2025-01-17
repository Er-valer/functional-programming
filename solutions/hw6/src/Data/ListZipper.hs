-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  , lLeft
  , lRight
  , lGenerator
  , lWrite
  , toList
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ l x r) = LZ (map f l) (f x) (map f r)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = lGenerator lLeft lRight
  extend f = fmap f . duplicate

lLeft, lRight :: ListZipper a -> ListZipper a

lLeft  (LZ (l : ls) x rs) = LZ ls l (x : rs)
lLeft  lz = lz

lRight (LZ ls x (r : rs)) = LZ (x : ls) r rs
lRight lz = lz

lGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lGenerator leftGen rightGen x = LZ (iterateTail leftGen x) x (iterateTail rightGen x)
  where iterateTail f = tail . iterate f

lWrite :: a -> ListZipper a -> ListZipper a
lWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take ((n - 1) `div` 2) ls) ++ [x] ++ take (n `div` 2) rs
