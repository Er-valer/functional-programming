module HW5.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompress,
                               defaultCompressParams)
import Codec.Serialise (deserialise, serialise)
import Control.Monad (foldM)
import Control.Monad.Except (liftEither, runExceptT, throwError)
import Data.ByteString as BS (ByteString, drop, fromStrict, index, length, pack, reverse, take,
                              toStrict, unpack)
import Data.Foldable (toList)
import qualified Data.Map as Map (Map, elems, empty, fromList, fromListWith, insertWith, keys,
                                  lookup, map, toList)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence as Seq (Seq, drop, empty, fromList, index, length, reverse, take)
import Data.Text as Text (Text, concat, drop, length, pack, reverse, singleton, strip, take,
                          toLower, toUpper, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction),
                 HiValue (..))
import Text.Read (readMaybe)


eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)

eval (HiExprValue value) = pure $ return value

eval (HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) [l, r]) = do
  evalL <- eval l
  case evalL of
    Left _                    -> pure evalL
    Right (HiValueBool False) -> pure evalL
    Right HiValueNull         -> pure evalL
    Right _                   -> eval r

eval (HiExprApply (HiExprValue (HiValueFunction HiFunOr)) [l, r]) = do
  evalL <- eval l
  case evalL of
    Left _                    -> pure evalL
    Right (HiValueBool False) -> eval r
    Right HiValueNull         -> eval r
    Right _                   -> pure evalL

eval (HiExprApply (HiExprValue (HiValueFunction HiFunIf)) [cond, l, r]) = do
  evalCond <- eval cond
  case evalCond of
    Left _                    -> pure evalCond
    Right (HiValueBool False) -> eval r
    Right (HiValueBool True)  -> eval l
    Right _                   -> pure $ throwError HiErrorInvalidArgument

eval (HiExprApply (HiExprValue exprValue) args) = do
  evaluated <- fmap (mapM liftEither) (mapM eval args)
  pure $ case evaluated of
    Left err     -> throwError err
    Right values ->
      case exprValue of
        HiValueFunction func -> evalFunction func values
        HiValueDict dict     -> evalKey dict values
        _                    -> evalSlice exprValue values

eval (HiExprApply inner args) = do
  evalInner <- eval inner
  case evalInner of
    Left _        -> pure evalInner
    Right success -> eval (HiExprApply (HiExprValue success) args)

eval (HiExprRun run) = do
  evalRun <- eval run
  case evalRun of
    Left _                    -> pure evalRun
    Right (HiValueAction act) -> pure <$> runAction act
    Right _                   -> pure $ throwError HiErrorInvalidArgument

eval (HiExprDict dict) = do
  evalKeys <- fmap (mapM liftEither) (mapM (eval . fst) dict)
  evalValues <- fmap (mapM liftEither) (mapM (eval . snd) dict)
  pure $ case (evalKeys, evalValues) of
    (Right keys, Right values) -> return (HiValueDict (Map.fromList (zip keys values)))
    (Right _, Left err)        -> throwError err
    (Left err, _)              ->  throwError err


evalFunction :: HiFun -> [HiValue] -> Either HiError HiValue

evalFunction HiFunAdd [HiValueNumber l, HiValueNumber r] = return (HiValueNumber (l + r))
evalFunction HiFunAdd [HiValueString l, HiValueString r] = return (HiValueString (l <> r))
evalFunction HiFunAdd [HiValueList l, HiValueList r] = return (HiValueList (l <> r))
evalFunction HiFunAdd [HiValueBytes l, HiValueBytes r] = return (HiValueBytes (l <> r))
evalFunction HiFunAdd [HiValueTime time, HiValueNumber n] =
  return (HiValueTime (addUTCTime (fromRational n) time))
evalFunction HiFunAdd [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunAdd _ = throwError HiErrorArityMismatch

evalFunction HiFunSub [HiValueNumber l, HiValueNumber r] = return (HiValueNumber (l - r))
evalFunction HiFunSub [HiValueTime l, HiValueTime r] =
  return (HiValueNumber $ toRational $ diffUTCTime l r)
evalFunction HiFunSub [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunSub _ = throwError HiErrorArityMismatch

evalFunction HiFunMul [HiValueNumber l, HiValueNumber r] = return (HiValueNumber (l * r))
evalFunction HiFunMul [semigroup, HiValueNumber r] =
  case maybeInt r of
    Just rI ->
      case semigroup of
        HiValueString str  -> return (HiValueString (stimes rI str))
        HiValueList list   -> return (HiValueList (stimes rI list))
        HiValueBytes bytes -> return (HiValueBytes (stimes rI bytes))
        _                  -> throwError HiErrorInvalidArgument
    Nothing -> throwError HiErrorInvalidArgument
evalFunction HiFunMul [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunMul _ = throwError HiErrorArityMismatch

evalFunction HiFunDiv [HiValueNumber l, HiValueNumber r]
  | r == 0 = throwError HiErrorDivideByZero
  | otherwise = return (HiValueNumber (l / r))
evalFunction HiFunDiv [HiValueString l, HiValueString r] =
  return (HiValueString (Text.concat [l, Text.pack "/", r]))
evalFunction HiFunDiv [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunDiv _ = throwError HiErrorArityMismatch

evalFunction HiFunAnd [HiValueBool b1, HiValueBool b2] = return (HiValueBool (b1 && b2))
evalFunction HiFunAnd [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunAnd _ = throwError HiErrorArityMismatch

evalFunction HiFunOr [HiValueBool b1, HiValueBool b2] = return (HiValueBool (b1 || b2))
evalFunction HiFunOr [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunOr _ = throwError HiErrorArityMismatch

evalFunction HiFunIf [HiValueBool cond, l, r] = return (if cond then l else r)
evalFunction HiFunIf [_, _, _] = throwError HiErrorInvalidArgument
evalFunction HiFunIf _ = throwError HiErrorArityMismatch

evalFunction HiFunNot [HiValueBool b] = return (HiValueBool (not b))
evalFunction HiFunNot [_] = throwError HiErrorInvalidArgument
evalFunction HiFunNot _ = throwError HiErrorArityMismatch

evalFunction HiFunEquals [x1, x2] = return (HiValueBool (x1 == x2))
evalFunction HiFunEquals _ = throwError HiErrorArityMismatch

evalFunction HiFunLessThan [x1, x2] = return (HiValueBool (x1 < x2))
evalFunction HiFunLessThan _ = throwError HiErrorArityMismatch

evalFunction HiFunGreaterThan [x1, x2] = return (HiValueBool (x1 > x2))
evalFunction HiFunGreaterThan _ = throwError HiErrorArityMismatch

evalFunction HiFunNotEquals [x1, x2] = return (HiValueBool (x1 /= x2))
evalFunction HiFunNotEquals _ = throwError HiErrorArityMismatch

evalFunction HiFunNotLessThan [x1, x2] = return (HiValueBool (x1 >= x2))
evalFunction HiFunNotLessThan _ = throwError HiErrorArityMismatch

evalFunction HiFunNotGreaterThan [x1, x2] = return (HiValueBool (x1 <= x2))
evalFunction HiFunNotGreaterThan _ = throwError HiErrorArityMismatch

evalFunction HiFunLength [HiValueString str] = return (HiValueNumber $ toRational $ Text.length str)
evalFunction HiFunLength [HiValueList list] = return (HiValueNumber $ toRational $ Seq.length list)
evalFunction HiFunLength [HiValueBytes bytes] = return (HiValueNumber $ toRational $ BS.length bytes)
evalFunction HiFunLength [_] = throwError HiErrorInvalidArgument
evalFunction HiFunLength _ = throwError HiErrorArityMismatch

evalFunction HiFunToUpper [HiValueString str] = return (HiValueString (toUpper str))
evalFunction HiFunToUpper [_] = throwError HiErrorInvalidArgument
evalFunction HiFunToUpper _ = throwError HiErrorArityMismatch

evalFunction HiFunToLower [HiValueString str] = return (HiValueString (toLower str))
evalFunction HiFunToLower [_] = throwError HiErrorInvalidArgument
evalFunction HiFunToLower _ = throwError HiErrorArityMismatch

evalFunction HiFunReverse [HiValueString str] = return (HiValueString (Text.reverse str))
evalFunction HiFunReverse [HiValueList list] = return (HiValueList (Seq.reverse list))
evalFunction HiFunReverse [HiValueBytes bytes] = return (HiValueBytes (BS.reverse bytes))
evalFunction HiFunReverse [_] = throwError HiErrorInvalidArgument
evalFunction HiFunReverse _ = throwError HiErrorArityMismatch

evalFunction HiFunTrim [HiValueString str] = return (HiValueString (Text.strip str))
evalFunction HiFunTrim [_] = throwError HiErrorInvalidArgument
evalFunction HiFunTrim _ = throwError HiErrorArityMismatch

evalFunction HiFunList args = return (HiValueList (Seq.fromList args))

evalFunction HiFunFold [HiValueFunction f, HiValueList sq] =
  case toList sq of
    [] -> return (HiValueList Seq.empty)
    [x] -> do
      case evalFunction f [x, x] of
        Left _  -> throwError HiErrorInvalidArgument
        Right _ -> return x
    (x:xs) -> do
      result <- runExceptT (foldM (\l r -> liftEither $ evalFunction f [l, r]) x xs)
      case result of
        Left err    -> throwError err
        Right value -> return value
evalFunction HiFunFold [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunFold _ = throwError HiErrorArityMismatch

evalFunction HiFunRange [HiValueNumber l, HiValueNumber r] =
  return (HiValueList (Seq.fromList $ map HiValueNumber $ takeWhile (<= r) $ iterate (+ 1) l))
evalFunction HiFunRange [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunRange _ = throwError HiErrorArityMismatch

evalFunction HiFunPackBytes [HiValueList list] =
  case mapM byteFromHiValue (toList list) of
    Just bytes -> return (HiValueBytes (BS.pack bytes))
    Nothing    -> throwError HiErrorInvalidArgument
evalFunction HiFunPackBytes [_] = throwError HiErrorInvalidArgument
evalFunction HiFunPackBytes _ = throwError HiErrorArityMismatch

evalFunction HiFunUnpackBytes [HiValueBytes bytes] =
  return (HiValueList (Seq.fromList $ map (HiValueNumber . toRational) (BS.unpack bytes)))
evalFunction HiFunUnpackBytes [_] = throwError HiErrorInvalidArgument
evalFunction HiFunUnpackBytes _ = throwError HiErrorArityMismatch

evalFunction HiFunEncodeUtf8 [HiValueString str] = return (HiValueBytes (encodeUtf8 str))
evalFunction HiFunEncodeUtf8 [_] = throwError HiErrorInvalidArgument
evalFunction HiFunEncodeUtf8 _ = throwError HiErrorArityMismatch

evalFunction HiFunDecodeUtf8 [HiValueBytes bytes] =
  case decodeUtf8' bytes of
    Left _    -> return HiValueNull
    Right str -> return (HiValueString str)
evalFunction HiFunDecodeUtf8 [_] = throwError HiErrorInvalidArgument
evalFunction HiFunDecodeUtf8 _ = throwError HiErrorArityMismatch

evalFunction HiFunZip [HiValueBytes bytes] =
  return (HiValueBytes (toStrict (compressWith params (fromStrict bytes))))
  where
    params = defaultCompressParams {compressLevel = bestCompression}
evalFunction HiFunZip [_] = throwError HiErrorInvalidArgument
evalFunction HiFunZip _ = throwError HiErrorArityMismatch

evalFunction HiFunUnzip [HiValueBytes bytes] =
  return (HiValueBytes (toStrict (decompress (fromStrict bytes))))
evalFunction HiFunUnzip [_] = throwError HiErrorInvalidArgument
evalFunction HiFunUnzip _ = throwError HiErrorArityMismatch

evalFunction HiFunSerialise [obj] = return (HiValueBytes $ toStrict $ serialise obj)
evalFunction HiFunSerialise _ = throwError HiErrorArityMismatch

evalFunction HiFunDeserialise [HiValueBytes bytes] = return (deserialise $ fromStrict bytes)
evalFunction HiFunDeserialise [_] = throwError HiErrorInvalidArgument
evalFunction HiFunDeserialise _ = throwError HiErrorArityMismatch

evalFunction HiFunRead [HiValueString path] =
  return (HiValueAction (HiActionRead (Text.unpack path)))
evalFunction HiFunRead [_] = throwError HiErrorInvalidArgument
evalFunction HiFunRead _ = throwError HiErrorArityMismatch

evalFunction HiFunWrite [HiValueString path, HiValueBytes bytes] =
  return (HiValueAction (HiActionWrite (Text.unpack path) bytes))
evalFunction HiFunWrite [HiValueString path, HiValueString str] =
  return (HiValueAction (HiActionWrite (Text.unpack path) (encodeUtf8 str)))
evalFunction HiFunWrite [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunWrite _ = throwError HiErrorArityMismatch

evalFunction HiFunMkDir [HiValueString path] =
  return (HiValueAction (HiActionMkDir (Text.unpack path)))
evalFunction HiFunMkDir [_] = throwError HiErrorInvalidArgument
evalFunction HiFunMkDir _ = throwError HiErrorArityMismatch

evalFunction HiFunChDir [HiValueString path] =
  return (HiValueAction (HiActionChDir (Text.unpack path)))
evalFunction HiFunChDir [_] = throwError HiErrorInvalidArgument
evalFunction HiFunChDir _ = throwError HiErrorArityMismatch

evalFunction HiFunParseTime [HiValueString str] =
  case (readMaybe (Text.unpack str) :: Maybe UTCTime) of
    Just time -> return (HiValueTime time)
    Nothing   -> return HiValueNull
evalFunction HiFunParseTime [_] = throwError HiErrorInvalidArgument
evalFunction HiFunParseTime _ = throwError HiErrorArityMismatch

evalFunction HiFunRand [HiValueNumber from, HiValueNumber to] =
  case (maybeInt from, maybeInt to) of
    (Just fromI, Just toI) -> return (HiValueAction (HiActionRand fromI toI))
    (_, _)                 -> throwError HiErrorInvalidArgument
evalFunction HiFunRand [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunRand _ = throwError HiErrorArityMismatch

evalFunction HiFunEcho [HiValueString str] = return (HiValueAction (HiActionEcho str))
evalFunction HiFunEcho [_]                 = throwError HiErrorInvalidArgument
evalFunction HiFunEcho _ = throwError HiErrorArityMismatch

evalFunction HiFunCount [HiValueList list] =
  return (HiValueDict (Map.map HiValueNumber (dictFromSeq list)))
evalFunction HiFunCount [HiValueBytes bytes] =
  return (HiValueDict (Map.map HiValueNumber (dictFromSeq bytesList)))
  where
    bytesList = map (HiValueNumber . toRational) (BS.unpack bytes)
evalFunction HiFunCount [HiValueString str] =
  return (HiValueDict (Map.map HiValueNumber (dictFromSeq textList)))
  where
    textList = map (HiValueString . Text.singleton) (Text.unpack str)
evalFunction HiFunCount [_] = throwError HiErrorInvalidArgument
evalFunction HiFunCount _ = throwError HiErrorArityMismatch

evalFunction HiFunKeys [HiValueDict dict] =
  return (HiValueList (Seq.fromList (Map.keys dict)))
evalFunction HiFunKeys [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunKeys _ = throwError HiErrorArityMismatch

evalFunction HiFunValues [HiValueDict dict] =
  return (HiValueList (Seq.fromList (Map.elems dict)))
evalFunction HiFunValues [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunValues _ = throwError HiErrorArityMismatch

evalFunction HiFunInvert [HiValueDict dict] =
  return (HiValueDict (Map.map (HiValueList . Seq.fromList) invertedDict))
  where
    invertedDict = Map.fromListWith (++) (map (\(x, y) -> (y, [x])) (Map.toList dict))
evalFunction HiFunInvert [_, _] = throwError HiErrorInvalidArgument
evalFunction HiFunInvert _ = throwError HiErrorArityMismatch


evalSlice :: HiValue -> [HiValue] -> Either HiError HiValue
evalSlice (HiValueString str) args = do
  s <- slice str args
  case s of
    Just res -> return (HiValueString res)
    Nothing  -> return HiValueNull

evalSlice (HiValueList list) args = do
  s <- slice list args
  case s of
    Just res ->
      case args of
        [_] -> return (Seq.index res 0)
        _   -> return (HiValueList res)
    Nothing -> return HiValueNull

evalSlice (HiValueBytes bytes) args = do
  s <- slice bytes args
  case s of
    Just res ->
      case args of
        [_] -> return (HiValueNumber (fromIntegral (BS.index res 0)))
        _   -> return (HiValueBytes res)
    Nothing -> return HiValueNull

evalSlice _ _ = throwError HiErrorInvalidFunction

evalKey :: Map.Map HiValue HiValue -> [HiValue] -> Either HiError HiValue
evalKey dict [key] =
  case Map.lookup key dict of
    Nothing  -> throwError HiErrorInvalidArgument
    Just val -> return val
evalKey _ _ = throwError HiErrorArityMismatch


-- Helper functions and classes

class Slicable a where
  size :: a -> Int
  simpleSlice :: a -> Int -> Int -> a

instance Slicable Text where
  size = Text.length
  simpleSlice str start end = Text.take (end - start) (Text.drop start str)

instance Slicable (Seq a) where
  size = Seq.length
  simpleSlice list start end = Seq.take (end - start) (Seq.drop start list)

instance Slicable ByteString where
  size = BS.length
  simpleSlice bytes start end = BS.take (end - start) (BS.drop start bytes)

slice :: Slicable a => a -> [HiValue] -> Either HiError (Maybe a)
slice slicable [HiValueNumber start, HiValueNumber end] =
  case (maybeInt start, maybeInt end) of
    (Just startI, Just endI) ->
      let
        len = size slicable
        adjustedStart = if startI < 0 then len + startI else startI
        adjustedEnd = if endI < 0 then len + endI else endI
      in
        return (Just (simpleSlice slicable adjustedStart adjustedEnd))
    (_, _) -> throwError HiErrorInvalidArgument

slice s [i@(HiValueNumber ind)] =
  if ind >= 0 && ind < toRational (size s)
    then slice s [i, HiValueNumber (ind + 1)]
    else return Nothing

slice s [start@(HiValueNumber _), HiValueNull] =
  slice s [start, HiValueNumber (toRational (size s))]

slice s [HiValueNull, end@(HiValueNumber _)] =
  slice s [HiValueNumber 0, end]

slice _ [_, _] = throwError HiErrorInvalidArgument
slice _ [_] = throwError HiErrorInvalidArgument
slice _ _ = throwError HiErrorArityMismatch

maybeInt :: Rational -> Maybe Int
maybeInt r | denominator r == 1 = Just (fromInteger (numerator r))
maybeInt _ = Nothing

byteFromHiValue :: HiValue -> Maybe Word8
byteFromHiValue (HiValueNumber r) =
  if r >= 0 && r <= 255
    then case maybeInt r of
      Just rI -> Just (fromIntegral rI)
      Nothing -> Nothing
    else Nothing
byteFromHiValue _ = Nothing

dictFromSeq :: (Ord a, Foldable t) => t a -> Map.Map a Rational
dictFromSeq = foldr (\x -> Map.insertWith (+) x 1) Map.empty
