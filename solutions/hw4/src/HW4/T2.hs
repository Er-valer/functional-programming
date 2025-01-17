{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Control.Applicative
import Control.Monad
import Numeric.Natural (Natural)

import qualified Data.Char
import Data.Foldable (Foldable (foldl'))
import Data.Functor (($>))
import HW4.T1 (ExceptState (..))
import HW4.Types


newtype ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P p) s =
  case runES p (0, s) of
    Error e          -> Error e
    Success (a :# _) -> Success a

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos + 1, s))
    _  -> Error (ErrorAtPos pos)

test :: (Char -> Bool) -> Parser Bool
test p = P $ ES $ \(pos, s) ->
  case s of
    []    -> Error (ErrorAtPos pos)
    (c:_) -> Success (p c :# (pos, s))

char :: Char -> Parser Char
char ch = mfilter (==ch) pChar

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P p1) <|> (P p2) = P $ ES $ \st->
    case runES p1 st of
      Error _ -> runES p2 st
      success -> success

-- No metohds
instance MonadPlus Parser

skipWS :: Parser ()
skipWS = void $ many (do
  isSpace <- test Data.Char.isSpace
  if isSpace
    then pChar
    else parseError)

pVal :: Parser Double
pVal = do
  signs <- many (char '-')
  whole <- some (mfilter Data.Char.isDigit pChar)
  dot <- optional (char '.')
  case dot of
    Nothing -> do
      pure $ signsToDouble signs * wholeToDouble whole
    Just _ -> do
      frac <- some (mfilter Data.Char.isDigit pChar)
      pure $ signsToDouble signs * (wholeToDouble whole + fracToDouble frac)
  where
    digToDouble ch = fromIntegral (fromEnum ch - fromEnum '0')
    wholeToDouble = foldl' (\xs x -> digToDouble x + xs * 10) 0
    fracToDouble = foldr (\x xs -> (digToDouble x + xs) / 10) 0
    signsToDouble = foldr (\_ xs -> (-1) * xs) 1

pBinary :: Parser (Expr -> Expr -> Prim Expr) -> Parser Expr -> Parser Expr
pBinary pOp pNextLvl = do
  skipWS
  left <- pNextLvl
  rights <- many (do
    skipWS
    op <- pOp
    skipWS
    right <- pNextLvl
    pure (op, right))
  pure $ foldl' (\l (op, r) -> Op (op l r)) left rights

pExpr :: Parser Expr
pExpr = do
  expr <- pSecondPriority
  skipWS
  pure expr

pSecondPriority :: Parser Expr
pSecondPriority = pBinary ((char '+' $> Add) <|> (char '-' $> Sub)) pFirstPriority

pFirstPriority :: Parser Expr
pFirstPriority = pBinary ((char '/' $> Div) <|> (char '*' $> Mul)) pZeroPriority

pZeroPriority :: Parser Expr
pZeroPriority = do
  skipWS
  isBrackets <- test (=='(')
  if isBrackets
    then do
      void $ char '('
      expr <- pExpr
      void $ char ')'
      pure expr
    else Val <$> pVal

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ do
  expr <- pExpr
  pEof
  pure expr
