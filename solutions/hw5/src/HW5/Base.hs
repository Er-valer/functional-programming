{-# LANGUAGE DeriveGeneric #-}
module HW5.Base
  ( HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , HiAction(..)
  , HiMonad(..)
  , hiFunPrettyName
  , hiFunParseName
  ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

newtype PrettyName = PrettyName {getPrettyName :: String}
newtype ParseName = ParseName {getParseName :: String}

data Info = Info {
  prettyName :: PrettyName,
  parseName  :: ParseName
}

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data HiValue =
    HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Generic)

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic)

instance Serialise HiFun
instance Serialise HiAction
instance Serialise HiValue

instance Eq HiValue where
  HiValueNumber n1 == HiValueNumber n2     = n1 == n2
  HiValueBool b1 == HiValueBool b2         = b1 == b2
  HiValueFunction f1 == HiValueFunction f2 = f1 == f2
  HiValueNull == HiValueNull               = True
  HiValueString s1 == HiValueString s2     = s1 == s2
  HiValueList l1 == HiValueList l2         = l1 == l2
  HiValueBytes bs1 == HiValueBytes bs2     = bs1 == bs2
  HiValueAction a1 == HiValueAction a2     = a1 == a2
  HiValueTime t1 == HiValueTime t2         = t1 == t2
  HiValueDict d1 == HiValueDict d2         = d1 == d2
  _ == _                                   = False

instance Ord HiValue where
  HiValueNumber n1 <= HiValueNumber n2     = n1 <= n2
  HiValueBool b1 <= HiValueBool b2         = b1 <= b2
  HiValueFunction f1 <= HiValueFunction f2 = f1 <= f2
  HiValueString s1 <= HiValueString s2     = s1 <= s2
  HiValueList l1 <= HiValueList l2         = l1 <= l2
  HiValueBytes bs1 <= HiValueBytes bs2     = bs1 <= bs2
  HiValueAction a1 <= HiValueAction a2     = a1 <= a2
  HiValueTime t1 <= HiValueTime t2         = t1 <= t2
  HiValueDict d1 <= HiValueDict d2         = d1 <= d2

  HiValueBool _ <= HiValueNumber _         = True
  _ <= _                                   = False

hiFunPrettyName :: HiFun -> String
hiFunPrettyName fun = getPrettyName $ prettyName $ hiFunInfo fun

hiFunParseName :: HiFun -> String
hiFunParseName fun = getParseName $ parseName $ hiFunInfo fun

hiFunInfo :: HiFun -> Info
hiFunInfo fun =
  case fun of
    HiFunDiv            -> Info (PrettyName "div") (ParseName "div")
    HiFunMul            -> Info (PrettyName "mul") (ParseName "mul")
    HiFunAdd            -> Info (PrettyName "add") (ParseName "add")
    HiFunSub            -> Info (PrettyName "sub") (ParseName "sub")
    HiFunNot            -> Info (PrettyName "not") (ParseName "not")
    HiFunAnd            -> Info (PrettyName "and") (ParseName "and")
    HiFunOr             -> Info (PrettyName "or") (ParseName "or")
    HiFunLessThan       -> Info (PrettyName "less-than") (ParseName "less-than")
    HiFunGreaterThan    -> Info (PrettyName "greater-than") (ParseName "greater-than")
    HiFunEquals         -> Info (PrettyName "equals") (ParseName "equals")
    HiFunNotLessThan    -> Info (PrettyName "not-less-than") (ParseName "not-less-than")
    HiFunNotGreaterThan -> Info (PrettyName "not-greater-than") (ParseName "not-greater-than")
    HiFunNotEquals      -> Info (PrettyName "not-equals") (ParseName "not-equals")
    HiFunIf             -> Info (PrettyName "if") (ParseName "if")
    HiFunLength         -> Info (PrettyName "length") (ParseName "length")
    HiFunToUpper        -> Info (PrettyName "to-upper") (ParseName "to-upper")
    HiFunToLower        -> Info (PrettyName "to-lower") (ParseName "to-lower")
    HiFunReverse        -> Info (PrettyName "reverse") (ParseName "reverse")
    HiFunTrim           -> Info (PrettyName "trim") (ParseName "trim")
    HiFunList           -> Info (PrettyName "list") (ParseName "list")
    HiFunRange          -> Info (PrettyName "range") (ParseName "range")
    HiFunFold           -> Info (PrettyName "fold") (ParseName "fold")
    HiFunPackBytes      -> Info (PrettyName "pack-bytes") (ParseName "pack-bytes")
    HiFunUnpackBytes    -> Info (PrettyName "unpack-bytes") (ParseName "unpack-bytes")
    HiFunEncodeUtf8     -> Info (PrettyName "encode-utf8") (ParseName "encode-utf8")
    HiFunDecodeUtf8     -> Info (PrettyName "decode-utf8") (ParseName "decode-utf8")
    HiFunZip            -> Info (PrettyName "zip") (ParseName "zip")
    HiFunUnzip          -> Info (PrettyName "unzip") (ParseName "unzip")
    HiFunSerialise      -> Info (PrettyName "serialise") (ParseName "serialise")
    HiFunDeserialise    -> Info (PrettyName "deserialise") (ParseName "deserialise")
    HiFunRead           -> Info (PrettyName "read") (ParseName "read")
    HiFunWrite          -> Info (PrettyName "write") (ParseName "write")
    HiFunMkDir          -> Info (PrettyName "mkdir") (ParseName "mkdir")
    HiFunChDir          -> Info (PrettyName "cd") (ParseName "cd")
    HiFunParseTime      -> Info (PrettyName "parse-time") (ParseName "parse-time")
    HiFunRand           -> Info (PrettyName "rand") (ParseName "rand")
    HiFunEcho           -> Info (PrettyName "echo") (ParseName "echo")
    HiFunCount          -> Info (PrettyName "count") (ParseName "count")
    HiFunKeys           -> Info (PrettyName "keys") (ParseName "keys")
    HiFunValues         -> Info (PrettyName "values") (ParseName "values")
    HiFunInvert         -> Info (PrettyName "invert") (ParseName "invert")
