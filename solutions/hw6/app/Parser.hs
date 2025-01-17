{-# LANGUAGE DeriveGeneric #-}
module Parser
  ( parseArgs
  , Args(..)
  ) where

import Control.Exception (Exception, throw)
import Control.Monad (unless)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper, info, long, metavar,
                            option, progDesc, value, (<**>))

newtype ArgumentError = ArgumentError [String] deriving (Show, Generic)
instance Exception ArgumentError

data Args = Args
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  , gridSize         :: Int
  , iterations       :: Int
  , seed             :: Int
  } deriving Show

parseArgs :: IO Args
parseArgs = do
  args <- execParser (info (pArgs <**> helper) (fullDesc <> progDesc "Comonad-19 infection simulation"))
  validateArgs args
  return args

pArgs :: Parser Args
pArgs = Args
  <$> option auto (long "prob"       <> metavar "DOUBLE" <> help "Infection probability")
  <*> option auto (long "incub"      <> metavar "INT"    <> help "Incubation period duration")
  <*> option auto (long "ill"        <> metavar "INT"    <> help "Illness duration")
  <*> option auto (long "immun"      <> metavar "INT"    <> help "Immunity duration")
  <*> option auto (long "grid-size"  <> metavar "INT"    <> help "Output grid size")
  <*> option auto (long "iterations" <> metavar "INT"    <> help "Number of simulation iterations")
  <*> option auto (long "seed"       <> metavar "INT"    <> help "Seed for random" <> value 0)

validateArgs :: Args -> IO ()
validateArgs (Args prob incub ill immun gs iters _) = do
  let errors = catMaybes
        [ validateProbability prob
        , validatePositive incub "Incubation period duration"
        , validatePositive ill "Illness duration"
        , validatePositive immun "Immunity duration"
        , validatePositive gs "Grid size"
        , validatePositive iters "Iterations"]
  unless (null errors) (throw $ ArgumentError errors)

validateProbability :: Double -> Maybe String
validateProbability p
  | p < 0 || p > 1 = Just "Infection probability must be between 0 and 1."
  | otherwise      = Nothing

validatePositive :: Int -> String -> Maybe String
validatePositive x name
  | x <= 0     = Just (name ++ " must be a positive integer.")
  | otherwise  = Nothing
