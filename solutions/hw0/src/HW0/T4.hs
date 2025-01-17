module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' = fix . (:)

map' :: (a -> b) -> [a] -> [b]
map' = fix (\rec f ar ->
  case ar of
    []     -> []
    (x:xs) -> f x : rec f xs)

fib :: Natural -> Natural
fib = fix (\rec prev cur n -> 
  if n == 0
    then cur
    else rec cur (prev + cur) (n - 1)) 1 0

fac :: Natural -> Natural
fac = fix (\rec n ->
  if n <= 1
    then 1
    else n * rec (n-1))
