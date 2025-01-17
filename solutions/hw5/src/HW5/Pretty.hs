module HW5.Pretty
  ( prettyValue
  ) where

import Data.ByteString (ByteString, unpack)
import Data.Foldable (toList)
import Data.Map (assocs)
import Data.Ratio (denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiValue (..), hiFunPrettyName)
import Prettyprinter (Doc, Pretty (pretty), braces, dquotes, enclose, hcat, parens, prettyList,
                      punctuate, viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

newtype PrettyHiValue = Pretty HiValue

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty . Pretty

instance Pretty PrettyHiValue where
  pretty (Pretty (HiValueNumber n)) =
    let
    (_, mbRepetendIdx) = fromRationalRepetendUnlimited n
    denom = denominator n
    num = numerator n
    in
      case mbRepetendIdx of
        Just _ -> prettyMixedNum num denom
        Nothing
          | denom == 1 -> viaShow num
          | otherwise  -> viaShow (fromRational n :: Double)

  pretty (Pretty (HiValueBool bool))
    | bool = pretty "true"
    | otherwise = pretty "false"

  pretty (Pretty (HiValueFunction func)) = pretty $ hiFunPrettyName func
  pretty (Pretty HiValueNull) = pretty "null"
  pretty (Pretty (HiValueString text)) = prettyString text
  pretty (Pretty (HiValueList list)) = prettyList (map Pretty (toList list))
  pretty (Pretty (HiValueBytes bytes)) = prettyBytes bytes
  pretty (Pretty (HiValueAction action)) = prettyAction action
  pretty (Pretty (HiValueTime time)) = pretty "parse-time" <> prettyArgs [(prettyString . show) time]
  pretty (Pretty (HiValueDict dict)) =
    braces $ hcat $ punctuate (pretty ", ") (map prettyEntry (assocs dict))

prettyEntry :: (HiValue, HiValue) -> Doc ann
prettyEntry (key, value) = (pretty. Pretty) key <> pretty ": " <> (pretty . Pretty) value

prettyAction :: HiAction -> Doc ann
prettyAction action =
  case action of
    HiActionRead path        -> pretty "read" <> prettyArgs [prettyString path]
    HiActionWrite path bytes -> pretty "write" <> prettyArgs [prettyString path, prettyBytes bytes]
    HiActionMkDir path       -> pretty "mkdir" <> prettyArgs [prettyString path]
    HiActionChDir path       -> pretty "cd" <> prettyArgs [prettyString path]
    HiActionCwd              -> pretty "cwd"
    HiActionNow              -> pretty "now"
    HiActionRand from to     -> pretty "rand" <> prettyArgs [pretty from, pretty to]
    HiActionEcho text        -> pretty "echo" <> prettyArgs [prettyString text]

prettyString :: Pretty a => a -> Doc ann
prettyString str = dquotes $ pretty str

prettyArgs :: [Doc ann] -> Doc ann
prettyArgs args = parens $ hcat $ punctuate (pretty ", ") args

prettyMixedNum :: Integer -> Integer -> Doc ann
prettyMixedNum num denom
  | r == 0          = viaShow q
  | q == 0 && r > 0 = pretty (show r ++ "/" ++ show denom)
  | q == 0 && r < 0 = pretty ("-" ++ show (-r) ++ "/" ++ show denom)
  | r > 0           = pretty (show q ++ " + " ++ show r ++ "/" ++ show denom)
  | otherwise       = pretty (show q ++ " - " ++ show (-r) ++ "/" ++ show denom)
  where
    (q, r) = quotRem num denom

prettyBytes :: ByteString -> Doc ann
prettyBytes bytes =
  bytesBrakets $ hcat $ punctuate (pretty " ") (map prettyByte (unpack bytes))
  where
    prettyByte = pretty . (printf "%02x" :: Word8 -> String)
    bytesBrakets = enclose (pretty "[# ") (pretty " #]")
