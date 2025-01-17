module HW5.Parser
  ( parse
  ) where

import Control.Monad (msum, void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString as BS (pack)
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Char as Char
import Data.List (intercalate)
import Data.Text as Text (pack)
import Data.Void (Void)
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiExpr (HiExprApply, HiExprDict, HiExprRun, HiExprValue),
                 HiFun (..), HiValue (..), hiFunParseName)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), Parsec, between, count, many,
                        manyTill, optional, runParser, satisfy, sepBy, sepBy1, skipMany, (<|>))
import Text.Megaparsec.Char (char, spaceChar, string)
import Text.Megaparsec.Char.Lexer (charLiteral, scientific, signed)
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pExpr <* eof) ""

pExpr :: Parser HiExpr
pExpr = trim $ makeExprParser pTerm pOperators

pOperators :: [[Operator Parser HiExpr]]
pOperators =
  [
    [
      pOperator InfixL HiFunMul (void (char '*')),
      pOperator InfixL HiFunDiv (try (char '/' >> notFollowedBy (char '=')))
    ],
    [
      pOperator InfixL HiFunAdd (void (char '+')),
      pOperator InfixL HiFunSub (void (char '-'))
    ],
    [
      pOperator InfixN HiFunEquals (void (string "==")),
      pOperator InfixN HiFunLessThan (try (char '<' >> notFollowedBy (char '='))),
      pOperator InfixN HiFunGreaterThan (try (char '>' >> notFollowedBy (char '='))),
      pOperator InfixN HiFunNotEquals (void (string "/=")),
      pOperator InfixN HiFunNotLessThan (void (string ">=")),
      pOperator InfixN HiFunNotGreaterThan (void (string "<="))
    ],
    [
      pOperator InfixR HiFunAnd (void (string "&&"))
    ],
    [
      pOperator InfixR HiFunOr (void (string "||"))
    ]
  ]

pOperator :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr)
  -> HiFun -> Parser () -> Operator Parser HiExpr
pOperator infx func parser = infx ((\l r -> applyFun func [l, r]) <$ parser)

pTerm :: Parser HiExpr
pTerm = trim $ do
  funcOrValue <- trim $ msum
    [
      pHiValue,
      roundBrackets pExpr,
      pList,
      pDict
    ]
  maybeArgs <- many (pArgs <|> (:[]) <$> pKey)
  maybeRun <- optional $ trim $ char '!'

  let apply = foldl HiExprApply funcOrValue maybeArgs
  case maybeRun of
    Just _ -> return (HiExprRun apply)
    _      -> return apply

pArgs :: Parser [HiExpr]
pArgs = trim $ roundBrackets $ pExpr `sepBy` char ','

pList :: Parser HiExpr
pList = trim $ applyFun HiFunList <$> squareBrackets (pExpr `sepBy` char ',')

pDict :: Parser HiExpr
pDict = trim $ HiExprDict <$> curlyBrackets (pEntry `sepBy` char ',')

pEntry :: Parser (HiExpr, HiExpr)
pEntry = trim $ (,) <$> pExpr <*> (char ':' >> pExpr)

pKey :: Parser HiExpr
pKey = HiExprValue . HiValueString . Text.pack <$> fmap (intercalate "-")
  (char '.' >> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'))

pHiValue :: Parser HiExpr
pHiValue = trim $ HiExprValue <$> msum
  [
    pHiValueFunction,
    pHiValueNumber,
    pHiValueBool,
    pHiValueNull,
    pHiValueString,
    pHiValueBytes,
    pHiValueAction
  ]

pHiValueFunction :: Parser HiValue
pHiValueFunction = trim $ HiValueFunction <$>
  msum ([
    HiFunNotEquals <$ string "not-equals",
    HiFunNotLessThan <$ string "not-less-than",
    HiFunNotGreaterThan <$ string "not-greater-than"
  ] ++ map (\f -> f <$ string (hiFunParseName f)) [minBound .. maxBound])

pHiValueNumber :: Parser HiValue
pHiValueNumber = trim $ HiValueNumber . toRational <$> signed pSkipWS scientific

pHiValueBool :: Parser HiValue
pHiValueBool = trim $ HiValueBool <$> msum
  [
    True <$ string "true",
    False <$ string "false"
  ]

pHiValueNull :: Parser HiValue
pHiValueNull = trim $ HiValueNull <$ string "null"

pHiValueString :: Parser HiValue
pHiValueString = trim $ HiValueString . Text.pack <$> (char '"' >> manyTill charLiteral (char '"'))

pHiValueBytes :: Parser HiValue
pHiValueBytes = trim $ HiValueBytes . BS.pack <$> bytesBrackets (trim $ many pByte)

pHiValueAction :: Parser HiValue
pHiValueAction = trim $ HiValueAction <$> msum
  [
    HiActionCwd <$ string "cwd",
    HiActionNow <$ string "now"
  ]

pSkipWS :: Parser ()
pSkipWS = skipMany spaceChar

pByte :: Parser Word8
pByte = trim $ do
  byte <- count 2 (satisfy Char.isHexDigit)
  return (read ("0x" ++ byte) :: Word8)

trim :: Parser a -> Parser a
trim p = (pSkipWS *> p) <* pSkipWS

roundBrackets :: Parser a -> Parser a
roundBrackets = between (char '(') (char ')')

squareBrackets :: Parser a -> Parser a
squareBrackets = between (char '[') (char ']')

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (char '{') (char '}')

bytesBrackets :: Parser a -> Parser a
bytesBrackets = between (string "[#") (string "#]")

applyFun :: HiFun -> [HiExpr] -> HiExpr
applyFun func = HiExprApply (HiExprValue . HiValueFunction $ func)
