module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import qualified Control.Monad

newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f aES = ES (\s -> 
  case runES aES s of 
    Error e -> Error e
    Success (aS :# st) -> Success (f aS :# st))

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState aESES = ES (\s -> 
  case runES aESES s of
    Error e -> Error e
    Success (aES :# st) -> runES aES st)

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (const (Error e))

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  (>>=) aES f = joinExceptState (mapExceptState f aES)

data EvaluationError = DivideByZero
  deriving Show

evalBinary :: Expr -> Expr -> (Double -> Double -> Prim Double) -> 
  (Double -> Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
evalBinary expr1 expr2 constr compute =
  do 
    evalExpr1 <- eval expr1
    evalExpr2 <- eval expr2
    let prim = constr evalExpr1 evalExpr2
    modifyExceptState (prim :)
    case prim of 
      Div _ 0 ->  throwExceptState DivideByZero
      _ -> wrapExceptState (compute evalExpr1 evalExpr2)

evalUnary :: Expr -> (Double -> Prim Double) -> 
  (Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
evalUnary expr constr compute =
  do
    evalExpr <- eval expr
    modifyExceptState (constr evalExpr :)
    wrapExceptState (compute evalExpr)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val cnst)     = wrapExceptState cnst
eval (Op (Add a b)) = evalBinary a b Add (+)
eval (Op (Sub a b)) = evalBinary a b Sub (-)
eval (Op (Mul a b)) = evalBinary a b Mul (*)
eval (Op (Div a b)) = evalBinary a b Div (/)
eval (Op (Abs a))   = evalUnary a Abs abs
eval (Op (Sgn a))   = evalUnary a Sgn signum
