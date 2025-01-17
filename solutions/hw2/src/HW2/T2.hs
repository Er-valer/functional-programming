module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr splitOnHelper ([] :| [])
  where
    splitOnHelper cur (part :| parts)
      | cur == sep = [] :| (part : parts)
      | otherwise  = (cur : part) :| parts

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (x :| xs) = foldl (\prev cur -> prev ++ [sep] ++ cur) x xs
