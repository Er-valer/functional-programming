{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import HW0.T1 (distrib)
import Data.Char (isSpace)

a :: (Either String a, Either String b)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))

a_whnf :: (Either String a, Either String b)
a_whnf = (Left str, Left str) 
  where str = "AB" ++ "CD" ++ "EF"
-- the outermost part is the data constructor (,)

b :: [Bool]
b = map isSpace "Hello, World"

b_whnf :: [Bool]
b_whnf = isSpace 'H' : map isSpace "ello, World" 
-- the outermost part is the data constructor (:)

c :: String
c = if (1 :: Int) > 0 || error "X" then "Y" else "Z"

c_whnf :: String
c_whnf = "Y"