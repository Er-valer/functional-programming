module Main (main) where

import HW3.T1
import HW3.T2
import HW3.T4

listFromList :: [a] -> List a
listFromList = foldr (:.) Nil

testT4 :: IO ()
testT4 = do
    print (runS (eval (2 + 3 * 5 - 7)) [])
    print (runS (eval ((2 * 3 * 4 / 8 + 1) / 4)) [])

testT2 :: IO ()
testT2 = do
    let list1 = listFromList [1, 2, 3]
    let list2 = listFromList [4, 5, 6]
    print (distList (list1, list2) :: List (Int, Int))
    print (distList (Nil :: List Int, list2))
    print (distList (list1, Nil :: List Int))

main :: IO ()
main = do
    testT2
    testT4


