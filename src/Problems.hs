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

prob1 :: Randomizable a => String -> Vars -> Exp a -> Problem a
prob1 name args e = (name , local updateOpts (asks solution))
  where
  args' = map (:[]) args
  updateOpts = \ f -> f { name = name , cases = [(map Var args' , e)] }
  solution = \ opts -> (opts , runEvo opts)

probn :: Randomizable a => String -> Cases a -> Problem a
probn name cases = (name , local updateOpts (asks solution))
  where
  updateOpts = \ f -> f { name = name , cases = cases }
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
hilbertBool = "hilbert-bool"

categories = [churchBool, churchNat, churchPair, churchList, combLog, hilbertBool]

cboolProbs :: Problems Comb
cboolProbs = probs churchBool
  [ prob1 "true"  "ab"  $ "a"
  , prob1 "false" "ab"  $ "b"
  , prob1 "and"   "pq"  $ "p" :@: "q" :@: "p"
  , prob1 "or"    "pq"  $ "p" :@: "p" :@: "q"
  , prob1 "if"    "pab" $ "p" :@: "a" :@: "b"
  , prob1 "not"   "pab" $ "p" :@: "b" :@: "a"
  , prob1 "xor"   "pq"  $ "p" :@: ("q" :@: _false :@: _true) :@: "q"
  ]

cnatProbs :: Problems Comb
cnatProbs = probs churchNat
  [ prob1 "zero"  "fx"   $ "x"
  , prob1 "one"   "fx"   $ "f" :@: "x"
  , prob1 "two"   "fx"   $ "f" :@: ("f" :@: "x")
  , prob1 "three" "fx"   $ "f" :@: ("f" :@: ("f" :@: "x"))
  , prob1 "four"  "fx"   $ "f" :@: "f" :@: ("f" :@: ("f" :@: "x"))
  , prob1 "succ"  "nfx"  $ "f" :@: ("n" :@: "f" :@: "x")
  , prob1 "plus"  "mnfx" $ "m" :@: "f" :@: ("n" :@: "f" :@: "x")
  , prob1 "mult"  "mnf"  $ "m" :@: ("n" :@: "f")
  , prob1 "exp"   "mn"   $ "n" :@: "m"
  ]

cpairProbs :: Problems Comb
cpairProbs = probs churchPair
  [ prob1 "pair"   "xyz" $ "z" :@: "x" :@: "y"
  , prob1 "first"  "p"   $ "p" :@: _true
  , prob1 "second" "p"   $ "p" :@: _false
  ]

clistProbs :: Problems Comb
clistProbs = probs churchList
  [ prob1 "nil"    "cn"   $ "n"
  , prob1 "isnil"  "l"    $ "l" :@: (_K :@: _K :@: _false) :@: _true
  , prob1 "cons"   "htcn" $ "c" :@: "h" :@: ("t" :@: "c" :@: "n")
  , prob1 "head"   "l"    $ "l" :@: _true :@: _false
  -- , prob1 "tail"   "lcn"  $ "l" :@: undefined :@: (_K :@: "n") _false
  ]

-- http://www.angelfire.com/tx4/cus/combinator/birds.html

combLogProbs :: Problems Comb
combLogProbs = probs combLog
 [ prob1 "B"     "abc"     $ "a" :@: ("b" :@: "c")
 , prob1 "B1"    "abcd"    $ "a" :@: ("b" :@: "c" :@: "d")
 , prob1 "B2"    "abcde"   $ "a" :@: ("b" :@: "c" :@: "d" :@: "e")
 , prob1 "B3"    "abcd"    $ "a" :@: ("b" :@: ("c" :@: "d"))
 , prob1 "C"     "abc"     $ "a" :@: "c" :@: "b"
 , prob1 "D"     "abcd"    $ "a" :@: "b" :@: ("c" :@: "d")
 , prob1 "D1"    "abcde"   $ "a" :@: "b" :@: "c" :@: ("d" :@: "e")
 , prob1 "D2"    "abcde"   $ "a" :@: ("b" :@: "c") :@: ("d" :@: "e")
 , prob1 "E"     "abcde"   $ "a" :@: "b" :@: ("c" :@: "d" :@: "e")
 , prob1 "E^"    "abcdefg" $ "a" :@: ("b" :@: "c" :@: "d") :@: ("e" :@: "f" :@: "g")
 , prob1 "F"     "abc"     $ "c" :@: "b" :@: "a"
 , prob1 "G"     "abcd"    $ "a" :@: "d" :@: ("b" :@: "c")
 , prob1 "H"     "abc"     $ "a" :@: "b" :@: "c" :@: "b"
 , prob1 "I"     "a"       $ "a"
 , prob1 "J"     "abcd"    $ "a" :@: "b" :@: ("a" :@: "d" :@: "c")
 , prob1 "K"     "ab"      $ "a"
 , prob1 "L"     "ab"      $ "a" :@: ("b" :@: "b")
 , prob1 "M"     "a"       $ "a" :@: "a"
 , prob1 "M2"    "ab"      $ "a" :@: "b" :@: ("a" :@: "b")
 , prob1 "O"     "ab"      $ "b" :@: ("a" :@: "b")
 , prob1 "Q"     "abc"     $ "b" :@: ("a" :@: "c")
 , prob1 "Q1"    "abc"     $ "a" :@: ("c" :@: "b")
 , prob1 "Q2"    "abc"     $ "b" :@: ("c" :@: "a")
 , prob1 "Q3"    "abc"     $ "c" :@: ("a" :@: "b")
 , prob1 "Q4"    "abc"     $ "c" :@: ("b" :@: "a")
 , prob1 "R"     "abc"     $ "b" :@: "c" :@: "a"
 , prob1 "S"     "abc"     $ "a" :@: "c" :@: ("b" :@: "c")
 , prob1 "T"     "ab"      $ "b" :@: "a"
 , prob1 "U"     "ab"      $ "b" :@: ("a" :@: "a" :@: "b")
 , prob1 "V"     "abc"     $ "c" :@: "a" :@: "b"
 , prob1 "W"     "ab"      $ "a" :@: "b" :@: "b"
 , prob1 "W1"    "ab"      $ "b" :@: "a" :@: "a"
 , prob1 "I*"    "ab"      $ "a" :@: "b"
 , prob1 "W*"    "abc"     $ "a" :@: "b" :@: "c" :@: "c"
 , prob1 "C*"    "abcd"    $ "a" :@: "b" :@: "d" :@: "c"
 , prob1 "R*"    "abcd"    $ "a" :@: "c" :@: "d" :@: "b"
 , prob1 "F*"    "abcd"    $ "a" :@: "d" :@: "c" :@: "b"
 , prob1 "V*"    "abcd"    $ "a" :@: "c" :@: "b" :@: "d"
 , prob1 "I**"   "abc"     $ "a" :@: "b" :@: "c"
 , prob1 "W**"   "abcd"    $ "a" :@: "b" :@: "c" :@: "d" :@: "d"
 , prob1 "C**"   "abcde"   $ "a" :@: "b" :@: "c" :@: "e" :@: "d"
 , prob1 "R**"   "abcde"   $ "a" :@: "b" :@: "d" :@: "e" :@: "c"
 , prob1 "F**"   "abcde"   $ "a" :@: "b" :@: "e" :@: "d" :@: "c"
 , prob1 "V**"   "abcde"   $ "a" :@: "b" :@: "e" :@: "c" :@: "d"
 , prob1 "KI"    "ab"      $ "b"
 , prob1 "KM"    "ab"      $ "b" :@: "b"
 , prob1 "C(KM)" "ab"      $ "a" :@: "a"
 ]

hboolProbs :: Problems Boolean
hboolProbs = probs hilbertBool
  [ probn "not"
    [ ([Prim True_b] , Prim False_b)
    , ([Prim False_b] , Prim True_b)
    ]
  , probn "and"
    [ ([Prim True_b, Prim True_b] , Prim True_b)
    , ([Prim False_b, "b"] , Prim False_b)
    , (["a", Prim False_b] , Prim False_b)
    ]
  , probn "or"
    [ ([Prim True_b, "b"] , Prim True_b)
    , (["a", Prim True_b] , Prim False_b)
    , ([Prim False_b, Prim False_b] , Prim False_b)
    ]
  , probn "xor"
    [ ([Prim True_b, Prim True_b] , Prim False_b)
    , ([Prim False_b, Prim True_b] , Prim True_b)
    , ([Prim True_b, Prim False_b] , Prim True_b)
    , ([Prim False_b, Prim False_b] , Prim False_b)
    ]
  ]

----------------------------------------------------------------------


