module HW3.T4
  ( State (..),
    Prim (..),
    Expr (..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    eval,
  )
where

import qualified Control.Monad
import HW3.T1

newtype State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f s = S (mapAnnotated f . runS s)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState st = S (\s -> let (st2 :# s2) = runS st s in runS st2 s2)

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) = Control.Monad.ap

instance Monad (State s) where
  (>>=) stA f = joinState (mapState f stA)

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show)

data Expr = Val Double | Op (Prim Expr)
  deriving (Show)

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

evalBinary :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> State [Prim Double] Double
evalBinary expr1 expr2 constr compute =
  do
    evalExpr1 <- eval expr1
    evalExpr2 <- eval expr2
    modifyState (\exprs -> constr evalExpr1 evalExpr2 : exprs)
    wrapState (compute evalExpr1 evalExpr2)

evalUnary :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> State [Prim Double] Double
evalUnary expr constr compute =
  do
    evalExpr <- eval expr
    modifyState (\exprs -> constr evalExpr : exprs)
    wrapState (compute evalExpr)

eval :: Expr -> State [Prim Double] Double
eval (Val cnst)     = wrapState cnst
eval (Op (Add a b)) = evalBinary a b Add (+)
eval (Op (Sub a b)) = evalBinary a b Sub (-)
eval (Op (Mul a b)) = evalBinary a b Mul (*)
eval (Op (Div a b)) = evalBinary a b Div (/)
eval (Op (Abs a))   = evalUnary a Abs abs
eval (Op (Sgn a))   = evalUnary a Sgn signum
