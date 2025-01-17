module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural (Natural)

data N = Z | S N

nplus :: N -> N -> N
nplus a Z     = a
nplus a (S b) = nplus (S a) b

nmult :: N -> N -> N
nmult _ Z     = Z
nmult a (S b) = nplus a (nmult a b)

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z (S _)     = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp a b =
  case nsub a b of
    Nothing    -> LT
    Just Z     -> EQ
    Just (S _) -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = nToNum n + 1

nEven :: N -> Bool
nEven Z         = True
nEven (S Z)     = False
nEven (S (S a)) = nEven a

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv a b =
  case nsub a b of
    Nothing -> Z
    Just n  -> S (ndiv n b)

nmod :: N -> N -> N
nmod a b =
  case nsub a b of
    Nothing -> a
    Just n  -> nmod n b
