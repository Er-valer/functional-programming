{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import Data.Type.Bool (If, type (||))
import Data.Type.Equality (type (==))
import GHC.TypeLits (Symbol)

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains _ '[] = 'False
  Contains e (x ': xs) = e == x || Contains e xs

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete _ '[] = '[]
  Delete e (x ': xs) = If (e == x) xs (x ': Delete e xs)

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add e set = If (Contains e set) set (e ': set)
