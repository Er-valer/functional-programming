module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap f
  where
    f Nothing  = mempty
    f (Just x) = x

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap f
  where
    f (Left x)  = (x, mempty)
    f (Right x) = (mempty, x)
