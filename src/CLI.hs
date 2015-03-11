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
  [ option "h" "help" "display this message"
      (NoArg (\ opts -> opts { category = "help" }))
  , option "d" "defaults" "print default options"
      (NoArg (\ opts -> opts { category = "defaults" }))
  , option "r" "random" "random program generation"
      (NoArg (\ opts -> opts { rand = True }))
  , option "n" "normalize" "partially normalize population"
      (NoArg (\ opts -> opts { normalize = True }))
  , option "c" "category" "category of problems"
      (OptArg (maybe id (\ str opts -> opts { category = str })) "string")
  , option "p" "problem" "problem in a category"
      (OptArg (maybe id (\ str opts -> opts { name = str })) "string")
  , option "s" "seed" "random number seed"
      (OptArg (maybe id (\ n opts -> opts { seed = read n })) "number")
  , option "a" "attempts" "number of runs"
      (OptArg (maybe id (\ n opts -> opts { attempts = read n })) "number")
  , option "p" "population" "population size"
      (OptArg (maybe id (\ n opts -> opts { popSize = read n })) "number")
  , option "e" "elitism" "elitism"
      (OptArg (maybe id (\ n opts -> opts { elitism = read n / 100.0 })) "percent")
  , option "i" "max-init-depth" "maximum initial population depth"
      (OptArg (maybe id (\ n opts -> opts { maxInitDepth = read n })) "number")
  , option "g" "max-genetic-depth" "maximum genetic operation depth"
      (OptArg (maybe id (\ n opts -> opts { maxGeneticDepth = read n })) "number")
  , option "u" "min-structure" "mininum structure"
      (OptArg (maybe id (\ n opts -> opts { minStruture = read n })) "number")
  , option "m" "mutation" "mutation rate"
      (OptArg (maybe id (\ n opts -> opts { mutationRate = read n / 100.0 })) "percent")
  ]

parseOpts :: IO (Options a, [String])
parseOpts = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOpts o, n)
    (_,_,errs) -> ioError . userError $ concat errs ++ usageBanner

----------------------------------------------------------------------

doption :: Show a => String -> String -> String -> a -> OptDescr ()
doption tag name desc val = Option tag [name] (OptArg (const ()) (show val)) desc

defaultOptions :: Options a -> [OptDescr ()]
defaultOptions opts =
  [ doption "s" "seed" "random number seed"
      (seed opts)
  , doption "a" "attempts" "number of runs"
      (attempts opts)
  , doption "p" "population" "population size"
      (popSize opts)
  , doption "e" "elitism" "elitism"
      (truncate (100 * elitism opts))
  , doption "i" "max-init-depth" "maximum initial population depth"
      (maxInitDepth opts)
  , doption "g" "max-genetic-depth" "maximum genetic operation depth"
      (maxGeneticDepth opts)
  , doption "u" "min-structure" "mininum structure"
      (minStruture opts)
  , doption "m" "mutation" "mutation rate"
      (truncate (100 * mutationRate opts))
  ]

----------------------------------------------------------------------

usageBanner = usageInfo "Usage: cgp [OPTION...]" options

categoryUsage = "--category[=string] " ++ categoryMustBe

categoryError cat = "category `" ++ cat ++ "' "
  ++ categoryMustBe ++ "\n"

categoryMustBe = "must be one of the following:\n"
 ++ intercalate ", " categories

nameError cat name names = "name `" ++ name
  ++ "' for category `" ++ cat
  ++ "' must be one of the following:\n"
  ++ intercalate ", " names

maxDepthError init cross =
     "Max initial depth (" ++ show init
  ++ ") must be greater than max genetic depth ("
  ++ show cross ++ ")"

printProb :: Options a -> Problems a -> IO ()
printProb opts@(null . name -> True) probs = evoProbs opts probs
printProb opts@(name -> name) probs = case lookup name probs of
  Just prob -> evoProb opts (name , prob)
  Nothing -> ioError . userError $ nameError (category opts) name names
  where names = map fst probs

defaultsBanner :: Options a -> String
defaultsBanner opts = usageInfo "Defaults: cgp [OPTION...]" (defaultOptions opts)

----------------------------------------------------------------------

main = do
  (opts , _) <- parseOpts
  if maxInitDepth opts > maxGeneticDepth opts
  then ioError . userError $ maxDepthError (maxInitDepth opts) (maxGeneticDepth opts)
  else case category opts of
    x | x == "help" || x == "" -> putStrLn $ usageBanner ++ "\n" ++ categoryUsage
    x | x == "defaults" -> putStrLn $ defaultsBanner opts
    x | x == churchBool -> printProb opts cboolProbs
    x | x == churchNat -> printProb opts cnatProbs
    x | x == churchPair -> printProb opts cpairProbs
    x | x == churchList -> printProb opts clistProbs
    x | x == combLog -> printProb opts combLogProbs
    unknown -> ioError . userError $ categoryError unknown

----------------------------------------------------------------------