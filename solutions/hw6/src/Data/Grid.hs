-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , gNeighbors
  , gWrite
  , gGenerator
  , prettyGrid
  ) where

import Control.Comonad (Comonad (..))

import Control.Monad (liftM2)
import Data.ListZipper (ListZipper (..), lGenerator, lLeft, lRight, lWrite, toList)
import Prettyprinter (Doc, Pretty, hcat, pretty, vsep)
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
  extract = extract . extract . unGrid
  duplicate = Grid . fmap gHorizontal . gVertical
  extend f = fmap f . duplicate

gUp, gDown, gLeft, gRight :: Grid a -> Grid a
gUp    (Grid g) = Grid (lLeft  g)
gDown  (Grid g) = Grid (lRight g)
gLeft  (Grid g) = Grid (fmap lLeft  g)
gRight (Grid g) = Grid (fmap lRight g)

gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical   = lGenerator gUp gDown

gNeighbors :: [Grid a -> Grid a]
gNeighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [gLeft, gRight]
        verticals   = [gUp, gDown]

gWrite :: a -> Grid a -> Grid a
gWrite x (Grid g) = Grid $ lWrite newLine g
  where oldLine = extract g
        newLine = lWrite x oldLine

gGenerator :: (a -> a) -> (a -> a) -> ListZipper a -> Grid a
gGenerator upGen downGen x = Grid (lGenerator (fmap upGen) (fmap downGen) x)

toLists :: Grid a -> Int -> [[a]]
toLists (Grid g) n = map (`toList` n) (toList g n)

prettyGrid :: Pretty a => Grid a -> Int -> Doc ann
prettyGrid grid n = vsep . map prettyRow $ toLists grid n
  where prettyRow = hcat . map pretty
