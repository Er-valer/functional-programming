module HW2.T1
  ( Tree (..)
  , tfoldr
  , treeToList
  ) where

data Tree a = Leaf | Branch Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ z Leaf             = z
tfoldr f z (Branch _ l v r) = tfoldr f (f v (tfoldr f z r)) l

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
