module HW6.T1
  ( BucketsArray,
    CHT (..),
    newCHT,
    getCHT,
    putCHT,
    sizeCHT,
    initCapacity,
    loadFactor,
  )
where

import Control.Concurrent.Classy (MonadConc, MonadSTM (readTVar), STM, atomically, modifyTVar,
                                  newTVar, readTVarConc, writeTVar)
import Control.Concurrent.Classy.STM (TArray, TVar)
import Control.Monad (when)
import Data.Array.Base (getElems, getNumElements, newArray, readArray, writeArray)
import Data.Hashable (Hashable (hash))

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]

type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v),
    chtSize    :: TVar stm Int
  }

newCHT :: (MonadConc m) => m (CHT (STM m) k v)
newCHT = atomically $ do
  buckets <- newTVar =<< newArray (0, initCapacity - 1) []
  size <- newTVar 0
  return (CHT buckets size)

getCHT :: (MonadConc m, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT key cht = atomically $ do
  (_, bucket) <- getBucket key =<< readTVar (chtBuckets cht)
  return (lookup key bucket)

putCHT :: (MonadConc m,  Hashable k) => k -> v -> CHT (STM m) k v -> m ()
putCHT key value cht = atomically $ do
  tryResizeCHT cht
  putCHTImpl key value cht

sizeCHT :: (MonadConc m) => CHT (STM m) k v -> m Int
sizeCHT cht = readTVarConc (chtSize cht)

getBucket :: (MonadSTM m, Hashable k) => k -> BucketsArray m k v -> m (Int, Bucket k v)
getBucket key buckets = do
  capacity <- getNumElements buckets
  let bucketId = mod (hash key) capacity
  bucket <- readArray buckets bucketId
  return (bucketId, bucket)

tryResizeCHT :: (MonadSTM m, Hashable k) => CHT m k v -> m ()
tryResizeCHT cht = do
  size <- readTVar (chtSize cht)
  capacity <- getNumElements =<< readTVar (chtBuckets cht)
  when (fromIntegral size >= fromIntegral capacity * loadFactor) $ do
    allElems <- mconcat <$> (getElems =<< readTVar (chtBuckets cht))
    writeTVar (chtBuckets cht) =<< newArray (0, capacity * 2 - 1) []
    writeTVar (chtSize cht) 0
    mapM_ (\(k, v) -> putCHTImpl k v cht) allElems

putCHTImpl :: (MonadSTM m, Hashable k) => k -> v -> CHT m k v -> m ()
putCHTImpl key value cht = do
  buckets <- readTVar (chtBuckets cht)
  (bucketId, bucket) <- getBucket key buckets
  writeArray buckets bucketId =<< insertOrReplace bucket
  where
    insertOrReplace b =
      case b of
        []                       -> modifyTVar (chtSize cht) (+ 1) >> return [(key, value)]
        ((k, _) : xs) | key == k -> return ((k, value) : xs)
        (x : xs)                 -> (x :) <$> insertOrReplace xs
