module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  , simulate
  ) where

import System.Random (RandomGen (split), StdGen, random)

import Control.Comonad (Comonad (extend), extract)
import Data.Grid (Grid (..), gGenerator, gNeighbors, gWrite)
import Data.ListZipper (lGenerator)
import Prettyprinter (Pretty (pretty))

data Config = Config
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

instance Pretty CellState where
  pretty Healthy      = pretty "_"
  pretty (Infected _) = pretty "i"
  pretty (Ill _)      = pretty "#"
  pretty (Immune _)   = pretty "@"

data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

instance Pretty Cell where
  pretty (Cell state _) = pretty state

type Comonad19Grid = Grid Cell

simulate :: Config -> StdGen -> [Comonad19Grid]
simulate config initGen = iterate (evolve config) initGrid
  where
    fstCell cell = Cell Healthy (fst $ split (cellRand cell))
    sndCell cell = Cell Healthy (snd $ split (cellRand cell))
    row = lGenerator fstCell sndCell (Cell Healthy initGen)
    healthyGrid = gGenerator fstCell sndCell row
    initGrid = gWrite (Cell (Infected (incubationPeriod config)) initGen) healthyGrid

isCarrier :: Cell -> Bool
isCarrier (Cell state _) =
  case state of
    Ill _      -> True
    Infected _ -> True
    _          -> False

neighborsCarriers :: Comonad19Grid -> Int
neighborsCarriers grid = length . filter isCarrier
  $ map (\direction -> extract $ direction grid) gNeighbors

tryInfect :: Config -> Comonad19Grid -> CellState
tryInfect config grid =
  snd $ tryInfectImpl config (neighborsCarriers grid) (cellRand (extract grid))

tryInfectImpl :: Config -> Int -> StdGen -> (StdGen, CellState)
tryInfectImpl _ 0 gen = (gen, Healthy)
tryInfectImpl config n gen =
  let (curP, nextGen) = random gen
  in if curP < probability config
    then (nextGen, Infected (incubationPeriod config))
    else tryInfectImpl config (n - 1) nextGen

rule :: Config -> Comonad19Grid -> Cell
rule config grid =
  let Cell state rand = extract grid
  in
    Cell (case state of
      Infected 0 -> Ill (illnessDuration config)
      Infected n -> Infected (n - 1)
      Ill 0      -> Immune (immunityDuration config)
      Ill n      -> Ill (n - 1)
      Immune 0   -> Healthy
      Immune n   -> Immune (n - 1)
      Healthy    -> tryInfect config grid) rand

evolve :: Config -> Comonad19Grid -> Comonad19Grid
evolve = extend . rule
