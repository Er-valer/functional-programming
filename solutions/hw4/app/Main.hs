module Main (main) where

import HW4.T1
import HW4.T2

testT1 :: IO()
testT1 = do
    print (runES (eval (1 / (10 - 5 * 2))) [])
    print (runES (eval (2 + 3 * 5 - 7)) [])

testT2 :: IO()
testT2 = do
    print (parseExpr " 2 +   --3 +   2    * ( 2 +   2)  /  1   ")
    print (parseExpr "2 * (1 + 3)")
    print (parseExpr "24 + Hello")
    print (parseExpr "3.14 + 1.618 * 2")
    print (1 + 6 /10 +  1 / 100 + 8 / 1000 :: Double)
    print ((1.610 + 8 / 1000 :: Double) == (1.618 :: Double))

main :: IO ()
main = do
    testT1
    testT2
