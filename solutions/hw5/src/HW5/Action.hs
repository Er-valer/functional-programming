{-# LANGUAGE DerivingVia #-}
module HW5.Action
  (
    HiPermission(..)
    , PermissionException(..)
    , HIO(..)
    , setOf
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Set (Set, fromList, member)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import HW5.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random (randomRIO)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

newtype PermissionException = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via ReaderT (Set HiPermission) IO

instance MonadIO HIO where
  liftIO action = HIO $ const action

instance HiMonad HIO where
  runAction (HiActionRead path) =
    (withPermissionCheck AllowRead . liftIO) $ hiRead path
  runAction (HiActionWrite path bytes) =
    (withPermissionCheck AllowWrite . liftIO) $ hiWrite path bytes
  runAction (HiActionMkDir path) =
    (withPermissionCheck AllowWrite . liftIO) $ hiMkDir path
  runAction (HiActionChDir path) =
    (withPermissionCheck AllowRead . liftIO) $ hiChiDir path
  runAction HiActionCwd =
    withPermissionCheck AllowRead . liftIO $ hiCwd
  runAction HiActionNow =
    withPermissionCheck AllowTime . liftIO $ hiNow
  runAction (HiActionRand from to) = liftIO $ hiRand from to
  runAction (HiActionEcho text) =
    (withPermissionCheck AllowWrite . liftIO) $ hiEcho text


withPermissionCheck :: HiPermission -> HIO a -> HIO a
withPermissionCheck permission action = HIO $ \permissions -> do
    if member permission permissions
      then runHIO action permissions
      else (throwIO . PermissionRequired) permission

hiRead :: FilePath -> IO HiValue
hiRead path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      ls <- listDirectory path
      return (HiValueList (Seq.fromList (map (HiValueString . pack) ls)))
    else do
      file <- BS.readFile path
      case decodeUtf8' file of
        Left _    -> return (HiValueBytes file)
        Right str -> return (HiValueString str)

hiWrite :: FilePath -> ByteString -> IO HiValue
hiWrite path bytes = HiValueNull <$ BS.writeFile path bytes

hiMkDir :: FilePath -> IO HiValue
hiMkDir path = HiValueNull <$ createDirectory path

hiChiDir :: FilePath -> IO HiValue
hiChiDir path = HiValueNull <$ setCurrentDirectory path

hiCwd :: IO HiValue
hiCwd = HiValueString . pack <$> getCurrentDirectory

hiNow :: IO HiValue
hiNow = HiValueTime <$> getCurrentTime

hiRand :: Int -> Int -> IO HiValue
hiRand from to = HiValueNumber . toRational <$> randomRIO (from, to)

hiEcho :: Text -> IO HiValue
hiEcho text = HiValueNull <$ print text

setOf :: [HiPermission] -> Set HiPermission
setOf = fromList
