module Main (main) where

import Data.Grid (prettyGrid)
import HW6.T3 (Comonad19Grid, Config (Config), simulate)
import Parser (Args (Args), parseArgs)
import Prettyprinter (Doc, Pretty (pretty), space, vsep, hcat)
import System.Random (mkStdGen)

main :: IO ()
main = do
  args <- parseArgs
  let
    (Args prob incub ill immun gs iters seed) = args
    gen = mkStdGen seed
    config = Config prob incub ill immun
    simulation = take iters (simulate config gen)
  mapM_ (print . prettyIteration gs) (zip [1..] simulation)

prettyIteration :: Int -> (Int, Comonad19Grid) -> Doc ann
prettyIteration size (it, grid) = vsep [prettyName, prettyGrid grid size, space]
  where prettyName = hcat [pretty "Iteration ", pretty it, pretty ":"]

