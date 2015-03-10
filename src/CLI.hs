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

type OptTrans a = Options a -> Options a

option :: String -> String -> String -> ArgDescr (OptTrans a) -> OptDescr (OptTrans a)
option tag name desc f = Option tag [name] f desc

options :: [OptDescr (OptTrans a)]
options =
  [ option "c" "category" "category of problem"
      (OptArg (maybe id (\ str opts -> opts { category = str })) "string")
  , option "n" "name" "name of problem"
      (OptArg (maybe id (\ str opts -> opts { name = str })) "string")
  , option "r" "random-seed" "random number seed"
      (OptArg (maybe id (\ n opts -> opts { seed = mkStdGen (read n) })) "number")
  , option "a" "attempts" "number of runs"
      (OptArg (maybe id (\ n opts -> opts { attempts = read n })) "number")
  , option "p" "population" "population size"
      (OptArg (maybe id (\ n opts -> opts { popSize = read n })) "number")
  , option "e" "elitism" "elitism"
      (OptArg (maybe id (\ n opts -> opts { elitism = read n / 100.0 })) "percent")
  , option "i" "max-init-depth" "maximum initial depth"
      (OptArg (maybe id (\ n opts -> opts { maxInitDepth = read n })) "number")
  , option "o" "max-cross-depth" "maximum crossover depth"
      (OptArg (maybe id (\ n opts -> opts { maxCrossDepth = read n })) "number")
  , option "s" "min-structure" "mininum structure"
      (OptArg (maybe id (\ n opts -> opts { minStruture = read n })) "number")
  , option "m" "mutation" "mutation rate"
      (OptArg (maybe id (\ n opts -> opts { mutationRate = read n / 100.0 })) "percent")
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
  case category opts of
    x | x == churchBool -> gens opts cboolProbs
    x | x == churchNat -> gens opts cnatProbs
    x | x == churchPair -> gens opts cpairProbs
    x | x == churchList -> gens opts clistProbs
    x | x == combLog -> gens opts combLogProbs
    unknown -> putStrLn $ "Unknown problem category: '" ++ unknown ++ "'"

----------------------------------------------------------------------