{-# LANGUAGE
    ViewPatterns
  , OverloadedStrings
  , ConstraintKinds
  #-}

module Main where
import Tree
import Exp
import Evo
import Problems
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import System.Random
import Data.List
import Data.Bifunctor
import System.Environment
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )

----------------------------------------------------------------------

options :: [OptDescr (Options a -> Options a)]
options =
  [ Option ['n']     ["name"]
      (OptArg (maybe id (\ name opts -> opts { name = name })) "string")
      "name of problem"
  , Option ['r']     ["random-seed"]
      (OptArg (maybe id (\ n opts -> opts { seed = mkStdGen (read n) })) "number")
      "random number seed"
  , Option ['a']     ["attempts"]
      (OptArg (maybe id (\ n opts -> opts { attempts = read n })) "number")
      "number of runs"
  , Option ['p']     ["population"]
      (OptArg (maybe id (\ n opts -> opts { popSize = read n })) "number")
      "population size"
  , Option ['e']     ["elitism"]
      (OptArg (maybe id (\ n opts -> opts { elitism = read n / 100.0 })) "percent")
      "elitism"
  , Option ['i']     ["max-init-depth"]
      (OptArg (maybe id (\ n opts -> opts { maxInitDepth = read n })) "number")
      "maximum initial depth"
  , Option ['c']     ["max-cross-depth"]
      (OptArg (maybe id (\ n opts -> opts { maxCrossDepth = read n })) "number")
      "maximum crossover depth"
  , Option ['s']     ["min-structure"]
      (OptArg (maybe id (\ n opts -> opts { minStruture = read n })) "number")
      "mininum structure"
  , Option ['m']     ["mutation"]
      (OptArg (maybe id (\ n opts -> opts { mutationRate = read n / 100.0 })) "percent")
      "mutation rate"
  ]

parseOpts :: IO (Options a, [String])
parseOpts = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOpts o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: cgp [OPTION...]"

main = do
  (opts , _) <- parseOpts
  gens opts cnatProbs

----------------------------------------------------------------------