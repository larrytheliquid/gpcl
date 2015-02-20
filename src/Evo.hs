{-# LANGUAGE
    ViewPatterns
  , OverloadedStrings
  #-}

module Main where
import Tree
import Exp
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import System.Random
import Data.List
import Data.Bifunctor

----------------------------------------------------------------------

maxInitDepth :: Int
maxInitDepth = 20

maxCrossDepth :: Int
maxCrossDepth = 17

popSize :: Int
popSize = 1000

maxGen :: Int
maxGen = 30

elitism :: Float
elitism = 0.3

mutationRate :: Float
mutationRate = 0.0

hillMutationRate :: Float
hillMutationRate = 0.0

----------------------------------------------------------------------

type Evo = ReaderT (Exp , Args) (State StdGen)
type Gen = Int

randInt :: Int -> Evo Int
randInt n = state $ randomR (0, pred n)

randFloat :: Evo Float
randFloat = state random

randBool :: Evo Bool
randBool = (0 ==) <$> randInt 2

randElem :: [a] -> Evo a
randElem xs = (xs !!) <$> randInt (length xs)

randZip :: Tree a -> Evo (Zipper a)
randZip t = locate t <$> randInt (size t)

mkIndiv :: Tree Comb -> Evo Indiv
mkIndiv t = do
  (e , args) <- ask
  return (score t args e)

----------------------------------------------------------------------

mutate :: (Enum a, Bounded a) => Tree a -> Evo (Tree a)
mutate t1 = do
  z <- randZip t1
  t2 <- randTree' (depth (currentTree z))
  return $ rootTree (replace t2 z)

crossover :: Tree a -> Tree a -> Evo (Tree a)
crossover t1 t2 = do
  z1 <- randZip t1
  z2 <- randZip t2
  return $ rootTree (replace (currentTree z2) z1)

randTree' :: (Enum a, Bounded a) => Int -> Evo (Tree a)
randTree' n = do
  b <- randBool
  if b || n <= 0
  then Leaf <$> randElem enum
  else Branch <$> randTree' (pred n) <*> randTree' (pred n)

randTree :: (Enum a, Bounded a) => Evo (Tree a)
randTree = randTree' maxInitDepth

-- map depth (fst (runState (replicateM 5 (randTree :: Evo (Tree Comb))) (mkStdGen 7)))
-- [1,10,0,2,2]

----------------------------------------------------------------------

type Indiv = (Tree Comb , Int)
type Population = [Indiv]

randIndiv :: Evo Indiv
randIndiv = do
  t <- randTree
  mkIndiv t

mutationByFitness :: Int -> Float
mutationByFitness 0 = 0.0
mutationByFitness 1 = hillMutationRate
mutationByFitness _ = mutationRate

mutateIndiv :: Indiv -> Evo Indiv
mutateIndiv x@(t , i) = do
  n <- randFloat
  if n < mutationByFitness i
  then mkIndiv =<< mutate t
  else return x

randIndivs :: Int -> Evo Population
randIndivs n | n <= 0 = return []
randIndivs n | otherwise = insertIndiv <$> randIndiv <*> randIndivs (pred n)

initial :: Evo Population
initial = randIndivs popSize

select :: Population -> Evo Indiv
select ts = do
  t1 <- randElem ts
  t2 <- randElem ts
  return $ if snd t1 <= snd t2 then t1 else t2

breed :: Population -> Evo Indiv
breed ts = do
  t1 <- fst <$> select ts
  t2 <- fst <$> select ts
  t' <- crossover t1 t2
  mkIndiv t'

insertIndiv :: Indiv -> Population -> Population
insertIndiv t ts = insertBy (\x y -> compare (snd x) (snd y)) t ts

tooLarge :: Indiv -> Bool
tooLarge t = depth (fst t) > maxCrossDepth

isSolution :: Indiv -> Bool
isSolution t = snd t == 0

mutateGen :: Population -> Evo Population
mutateGen = foldM (\xs x -> flip insertIndiv xs <$> mutateIndiv x) []

crossoverGen :: Population -> Population -> Evo Population
crossoverGen ts ts' | length ts <= length ts' = return ts'
crossoverGen ts ts' | otherwise = do
  t' <- breed ts
  if tooLarge t'
  then crossoverGen ts ts'
  else crossoverGen ts (insertIndiv t' ts')

nextGen :: Population -> Population -> Evo Population
nextGen ts ts' = mutateGen =<< crossoverGen ts ts'

evolve :: Gen -> Population -> Evo (Gen , Population)
evolve n ts | n >= maxGen || isSolution (head ts) = return (n , ts)
-- evolve n ts | otherwise = evolve (succ n) =<< initial
evolve n ts | otherwise = evolve (succ n) =<< nextGen ts elite
  where
  -- elite = take 1 ts
  elite = take (truncate (fromIntegral (length ts) * elitism)) ts

evo :: Evo (Gen , Population)
evo = evolve 0 =<< initial

runEvo :: Exp -> Args -> Int -> (Gen , Population)
runEvo e args i = fst $ runState (runReaderT evo (e , args)) (mkStdGen i)

----------------------------------------------------------------------

type Vars = String
type Problem = [Int] -> Sol

data Sol = Sol
  { name   :: String
  , args   :: [String]
  , target :: Exp
  , runs   :: [(Gen , [Int])]
  } deriving Show

prob :: String -> Vars -> Exp -> Problem
prob name args e rs = Sol
  { name = name
  , args = args'
  , target = e
  , runs = map runner rs
  } where
  args' = map (:[]) args
  runner = bimap id (map snd) . runEvo e args'

attempts = 10
seed = 199

-- printAttempt :: (Gen , [Int]) -> IO ()
-- printAttempt (n , xs) = do
--   putStrLn $ "Generation " ++ show n
--   putStrLn $ show xs

printAttempts :: Sol -> IO ()
printAttempts sol = do
  putStrLn $ name sol ++ " : " ++ (show . sort . map fst) (runs sol) ++ " = " ++ show (sum (map fst (runs sol)))

gens :: [Problem] -> IO ()
gens probs = do
  let rs = take attempts $ randoms (mkStdGen seed)
  let ns = map (\f -> f rs) probs
  mapM_ printAttempts ns
  -- mapM_ printAttempt (sortBy (\x y -> compare (fst y) (fst x)) ns)

main = gens pairProbs

----------------------------------------------------------------------

-- http://en.wikipedia.org/wiki/Church_encoding#Church_Booleans
-- http://www.iep.utm.edu/lambda-calculi

boolProbs =
  [ prob "true"  "ab"  $ "a"
  , prob "false" "ab"  $ "b"
  , prob "and"   "pq"  $ "p" :@: "q" :@: "p"
  , prob "or"    "pq"  $ "p" :@: "p" :@: "q"
  , prob "if"    "pab" $ "p" :@: "a" :@: "b"
  , prob "not"   "pab" $ "p" :@: "b" :@: "a"
  , prob "xor"   "pq"  $ "p" :@: ("q" :@: _false :@: _true) :@: "q"
  ]

natProbs =
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

pairProbs =
  [ prob "pair"   "xyz" $ "z" :@: "x" :@: "y"
  , prob "first"  "p"   $ "p" :@: _true
  , prob "second" "p"   $ "p" :@: _false
  ]

-- http://www.angelfire.com/tx4/cus/combinator/birds.html

combLogProbs =
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


