module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

-- a -> f@(a -> Void) -> Void
doubleNeg :: a -> Not (Not a)
doubleNeg a f = f a

-- f@(((a -> Void) -> Void) -> Void) -> a -> Void
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f a = f (doubleNeg a)
