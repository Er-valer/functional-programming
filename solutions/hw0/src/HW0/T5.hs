module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ zero = zero

ns :: Nat a -> Nat a
ns x inc zero = inc (x inc zero)

nplus :: Nat a -> Nat a -> Nat a
nplus a b inc = a inc . b inc

nmult :: Nat a -> Nat a -> Nat a
nmult a b = a . b

nFromNatural :: Natural -> Nat a
nFromNatural n inc zero
  | n == 0 = zero
  | otherwise = inc (nFromNatural (n - 1) inc zero)

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
