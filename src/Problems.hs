{-# LANGUAGE
    ViewPatterns
  , OverloadedStrings
  , ConstraintKinds
  #-}

module Problems where
import Tree
import Exp
import Evo
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import System.Random
import Data.List
import Data.Bifunctor

----------------------------------------------------------------------

type Vars = String
type Problem a = (String , Reader (Options a) (Sol a (Population a)))
type Problems a = [Problem a]
type Sol a b = (Options a , [(Gen , b)])

prob :: Randomizable a => String -> Vars -> Exp a -> Problem a
prob name args e = (name , local updateOpts (asks solution))
  where
  args' = map (:[]) args
  updateOpts = \ f -> f { name = name , cases = [(map Var args' , e)] }
  solution = \ opts -> (opts , runEvo opts)

probs :: Randomizable a => String -> [Problem a] -> Problems a
probs category = map (bimap id updateOpts)
  where
  updateOpts = local (\ opts -> opts { category = category })

printOverview :: Sol a b -> IO ()
printOverview (opts , sols) = do
  putStrLn $ (name opts) ++ " : " ++ show (map fst sorted) ++ " = " ++ show (sum (map fst sols))
  -- putStrLn $ show $ (snd . head) sorted
  where sorted = sortBy (\ x y -> compare (fst x) (fst y)) sols

evoProbs :: Options a -> Problems a -> IO ()
evoProbs opts probs = mapM_ printOverview sols
  where sols = map (flip runReader opts . snd) probs

----------------------------------------------------------------------

printDetail :: (Gen , Population a) -> IO ()
printDetail (gen , (map snd -> ts)) = do
  putStrLn $ "Final Generation: " ++ show gen
  putStrLn $ show ts

printDetails :: Sol a (Population a) -> IO ()
printDetails (opts , sols) = mapM_ printDetail sorted
  where sorted = sortBy (\ x y -> compare (fst x) (fst y)) sols

evoProb :: Options a -> Problem a -> IO ()
evoProb opts prob = printDetails sol
  where sol = runReader (snd prob) opts

----------------------------------------------------------------------

-- http://en.wikipedia.org/wiki/Church_encoding#Church_Booleans
-- http://www.iep.utm.edu/lambda-calculi

churchBool = "church-bool"
churchNat = "church-nat"
churchPair = "church-pair"
churchList = "church-list"
combLog = "comb-log"

categories = [churchBool, churchNat, churchPair, churchList, combLog]

cboolProbs :: Problems Comb
cboolProbs = probs churchBool
  [ prob "true"  "ab"  $ "a"
  , prob "false" "ab"  $ "b"
  , prob "and"   "pq"  $ "p" :@: "q" :@: "p"
  , prob "or"    "pq"  $ "p" :@: "p" :@: "q"
  , prob "if"    "pab" $ "p" :@: "a" :@: "b"
  , prob "not"   "pab" $ "p" :@: "b" :@: "a"
  , prob "xor"   "pq"  $ "p" :@: ("q" :@: _false :@: _true) :@: "q"
  ]

cnatProbs :: Problems Comb
cnatProbs = probs churchNat
  [ prob "zero"  "fx"   $ "x"
  , prob "one"   "fx"   $ "f" :@: "x"
  , prob "two"   "fx"   $ "f" :@: ("f" :@: "x")
  , prob "three" "fx"   $ "f" :@: ("f" :@: ("f" :@: "x"))
  , prob "four"  "fx"   $ "f" :@: "f" :@: ("f" :@: ("f" :@: "x"))
  , prob "succ"  "nfx"  $ "f" :@: ("n" :@: "f" :@: "x")
  , prob "plus"  "mnfx" $ "m" :@: "f" :@: ("n" :@: "f" :@: "x")
  , prob "mult"  "mnf"  $ "m" :@: ("n" :@: "f")
  , prob "exp"   "mn"   $ "n" :@: "m"
  ]

cpairProbs :: Problems Comb
cpairProbs = probs churchPair
  [ prob "pair"   "xyz" $ "z" :@: "x" :@: "y"
  , prob "first"  "p"   $ "p" :@: _true
  , prob "second" "p"   $ "p" :@: _false
  ]

clistProbs :: Problems Comb
clistProbs = probs churchList
  [ prob "nil"    "cn"   $ "n"
  , prob "isnil"  "l"    $ "l" :@: (_K :@: _K :@: _false) :@: _true
  , prob "cons"   "htcn" $ "c" :@: "h" :@: ("t" :@: "c" :@: "n")
  , prob "head"   "l"    $ "l" :@: _true :@: _false
  -- , prob "tail"   "lcn"  $ "l" :@: undefined :@: (_K :@: "n") _false
  ]

-- http://www.angelfire.com/tx4/cus/combinator/birds.html

combLogProbs :: Problems Comb
combLogProbs = probs combLog
 [ prob "B"     "abc"     $ "a" :@: ("b" :@: "c")
 , prob "B1"    "abcd"    $ "a" :@: ("b" :@: "c" :@: "d")
 , prob "B2"    "abcde"   $ "a" :@: ("b" :@: "c" :@: "d" :@: "e")
 , prob "B3"    "abcd"    $ "a" :@: ("b" :@: ("c" :@: "d"))
 , prob "C"     "abc"     $ "a" :@: "c" :@: "b"
 , prob "D"     "abcd"    $ "a" :@: "b" :@: ("c" :@: "d")
 , prob "D1"    "abcde"   $ "a" :@: "b" :@: "c" :@: ("d" :@: "e")
 , prob "D2"    "abcde"   $ "a" :@: ("b" :@: "c") :@: ("d" :@: "e")
 , prob "E"     "abcde"   $ "a" :@: "b" :@: ("c" :@: "d" :@: "e")
 , prob "E^"    "abcdefg" $ "a" :@: ("b" :@: "c" :@: "d") :@: ("e" :@: "f" :@: "g")
 , prob "F"     "abc"     $ "c" :@: "b" :@: "a"
 , prob "G"     "abcd"    $ "a" :@: "d" :@: ("b" :@: "c")
 , prob "H"     "abc"     $ "a" :@: "b" :@: "c" :@: "b"
 , prob "I"     "a"       $ "a"
 , prob "J"     "abcd"    $ "a" :@: "b" :@: ("a" :@: "d" :@: "c")
 , prob "K"     "ab"      $ "a"
 , prob "L"     "ab"      $ "a" :@: ("b" :@: "b")
 , prob "M"     "a"       $ "a" :@: "a"
 , prob "M2"    "ab"      $ "a" :@: "b" :@: ("a" :@: "b")
 , prob "O"     "ab"      $ "b" :@: ("a" :@: "b")
 , prob "Q"     "abc"     $ "b" :@: ("a" :@: "c")
 , prob "Q1"    "abc"     $ "a" :@: ("c" :@: "b")
 , prob "Q2"    "abc"     $ "b" :@: ("c" :@: "a")
 , prob "Q3"    "abc"     $ "c" :@: ("a" :@: "b")
 , prob "Q4"    "abc"     $ "c" :@: ("b" :@: "a")
 , prob "R"     "abc"     $ "b" :@: "c" :@: "a"
 , prob "S"     "abc"     $ "a" :@: "c" :@: ("b" :@: "c")
 , prob "T"     "ab"      $ "b" :@: "a"
 , prob "U"     "ab"      $ "b" :@: ("a" :@: "a" :@: "b")
 , prob "V"     "abc"     $ "c" :@: "a" :@: "b"
 , prob "W"     "ab"      $ "a" :@: "b" :@: "b"
 , prob "W1"    "ab"      $ "b" :@: "a" :@: "a"
 , prob "I*"    "ab"      $ "a" :@: "b"
 , prob "W*"    "abc"     $ "a" :@: "b" :@: "c" :@: "c"
 , prob "C*"    "abcd"    $ "a" :@: "b" :@: "d" :@: "c"
 , prob "R*"    "abcd"    $ "a" :@: "c" :@: "d" :@: "b"
 , prob "F*"    "abcd"    $ "a" :@: "d" :@: "c" :@: "b"
 , prob "V*"    "abcd"    $ "a" :@: "c" :@: "b" :@: "d"
 , prob "I**"   "abc"     $ "a" :@: "b" :@: "c"
 , prob "W**"   "abcd"    $ "a" :@: "b" :@: "c" :@: "d" :@: "d"
 , prob "C**"   "abcde"   $ "a" :@: "b" :@: "c" :@: "e" :@: "d"
 , prob "R**"   "abcde"   $ "a" :@: "b" :@: "d" :@: "e" :@: "c"
 , prob "F**"   "abcde"   $ "a" :@: "b" :@: "e" :@: "d" :@: "c"
 , prob "V**"   "abcde"   $ "a" :@: "b" :@: "e" :@: "c" :@: "d"
 , prob "KI"    "ab"      $ "b"
 , prob "KM"    "ab"      $ "b" :@: "b"
 , prob "C(KM)" "ab"      $ "a" :@: "a"
 ]

----------------------------------------------------------------------


