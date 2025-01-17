module Main (main) where

import HW2.T1(Tree(..), treeToList)
import HW2.T3
import HW2.T2
import HW2.T4
import Data.List.NonEmpty (NonEmpty ((:|)))


newtype Sum a = Sum a deriving (Show)
instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)
instance Num a => Monoid (Sum a) where
  mempty = Sum 0

testT1 :: IO()
testT1 = do
  let tree1 = Branch 3 (Branch 1 Leaf 1 Leaf) 2 (Branch 1 Leaf 3 Leaf) :: Tree Int
  let tree2 = Branch 3 (Branch 1 Leaf 5 Leaf) 6 (Branch 1 Leaf 7 Leaf) :: Tree Int
  let tree = Branch 7 tree1 4 tree2
  print (treeToList tree)
  print (treeToList tree1)
  print (treeToList tree2)

testT2 :: IO()
testT2 = do
  let list = "aaa" :| ["bbb", "ccc", "ddd"]
  print (joinWith '.' list)
  print (splitOn '.' (joinWith '.' list))

testT3 :: IO()
testT3 = do
  let list = [Just "mo", Nothing, Nothing, Just "no", Just "id"]
  print (mcat list)
  let list2 = [Left (Sum (3 :: Int)), Right [1,2,3], Left (Sum (5 :: Int)), Right [4,5]]
  print (epart list2 :: (Sum Int, [Int]))

testT4 :: IO()
testT4 = do
  print (DS "person" <> DS "address" <> DS "city")
  print (DS "" <> DS "address" <> DS "city" <> DS "")



main :: IO()
main = do
    testT1
    testT2
    testT3
    testT4
