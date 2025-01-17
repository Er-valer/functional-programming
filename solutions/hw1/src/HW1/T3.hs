module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                     = 0
tsize (Branch (size, _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                       = 0
tdepth (Branch (_, depth) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember value (Branch _ left key right) = (value == key) ||
    if value > key
    then tmember value right
    else tmember value left

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left key right = Branch (size, depth) left key right
  where
    size = tsize left + tsize right + 1
    depth = max (tdepth left) (tdepth right) + 1

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = Leaf
rotateLeft tree@(Branch _ left key right) =
    case right of
      Leaf -> tree
      Branch _ rleft rkey rright ->
        mkBranch (mkBranch left key rleft) rkey rright

rotateRight :: Tree a -> Tree a
rotateRight Leaf = Leaf
rotateRight tree@(Branch _ left key right) =
    case left of
      Leaf -> tree
      Branch _ lleft lkey lright ->
        mkBranch lleft lkey (mkBranch lright key right)

rotateLeftRight :: Tree a -> Tree a
rotateLeftRight Leaf = Leaf
rotateLeftRight (Branch _ left key right) =
  rotateRight (mkBranch (rotateLeft left) key right)

rotateRightLeft :: Tree a -> Tree a
rotateRightLeft Leaf = Leaf
rotateRightLeft (Branch _ left key right) =
  rotateLeft (mkBranch left key (rotateRight right))

diff :: Tree a -> Int
diff Leaf                    = 0
diff (Branch _ left _ right) = tdepth left - tdepth right

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance tree@(Branch _ left _ right)
  | abs (diff tree) < 2 = tree
  | diff tree == -2 =
      if diff right <= 0
      then rotateLeft tree
      else rotateRightLeft tree
  | otherwise =
      if diff left >= 0
      then rotateRight tree
      else rotateLeftRight tree

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert value Leaf = Branch (1, 1) Leaf value Leaf
tinsert value tree@(Branch _ left key right)
  | value == key = tree
  | value > key = balance (mkBranch left key (tinsert value right))
  | otherwise = balance (mkBranch (tinsert value left) key right)

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
