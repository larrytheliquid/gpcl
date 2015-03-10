{-# LANGUAGE
    ViewPatterns
  , OverloadedStrings
  , ConstraintKinds
  #-}

module Evo where
import Tree
import Exp
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import System.Random
import Data.List
import Data.Bifunctor

----------------------------------------------------------------------

data Options a = Options
  { name :: String
  , maxInitDepth :: Int
  , maxCrossDepth :: Int
  , popSize :: Int
  , maxGen :: Int
  , elitism :: Float
  , mutationRate :: Float
  , minStruture :: Int
  , attempts :: Int
  , seed :: StdGen
  , cases :: Cases a
  }

----------------------------------------------------------------------

type Evo a = ReaderT (Options a) (State StdGen)
type Gen = Int
type Indiv a = (Tree a , Int)
type Population a = [Indiv a]

randInt :: Int -> Evo a Int
randInt n = state $ randomR (0, pred n)

randFloat :: Evo a Float
randFloat = state random

randBool :: Evo a Bool
randBool = (0 ==) <$> randInt 2

randElem :: [b] -> Evo a b
randElem xs = (xs !!) <$> randInt (length xs)

randZip :: Tree a -> Evo a (Zipper a)
randZip t = locate t <$> randInt (size t)

mkIndiv :: Scorable a => Tree a -> Evo a (Indiv a)
mkIndiv t = do
  minStruture <- asks minStruture
  cases <- asks cases
  return (score minStruture t cases)

----------------------------------------------------------------------

mutate :: Enumerable a => Tree a -> Evo a (Tree a)
mutate t1 = do
  z <- randZip t1
  t2 <- randTree' (depth (currentTree z))
  return $ rootTree (replace t2 z)

crossover :: Tree a -> Tree a -> Evo a (Tree a)
crossover t1 t2 = do
  z1 <- randZip t1
  z2 <- randZip t2
  return $ rootTree (replace (currentTree z2) z1)

randTree' :: Enumerable a => Int -> Evo a (Tree a)
randTree' n = do
  b <- randBool
  if b || n <= 0
  then Leaf <$> randElem enum
  else Branch <$> randTree' (pred n) <*> randTree' (pred n)

randTree :: Enumerable a => Evo a (Tree a)
randTree = randTree' =<< asks maxInitDepth

-- map depth (fst (runState (replicateM 5 (randTree :: Evo (Tree Comb))) (mkStdGen 7)))
-- [1,10,0,2,2]

----------------------------------------------------------------------

type Randomizable a = (Enum a, Bounded a, Eq a, Contractible a)

randIndiv :: Randomizable a => Evo a (Indiv a)
randIndiv = do
  t <- randTree
  mkIndiv t

mutationByFitness :: Int -> Evo a Float
mutationByFitness 0 = return 0.0
mutationByFitness _ = asks mutationRate

mutateIndiv :: Randomizable a => Indiv a -> Evo a (Indiv a)
mutateIndiv x@(t , i) = do
  n  <- randFloat
  n' <- mutationByFitness i
  if n < n'
  then mkIndiv =<< mutate t
  else return x

randIndivs :: Randomizable a => Int -> Evo a (Population a)
randIndivs n | n <= 0 = return []
randIndivs n | otherwise = insertIndiv <$> randIndiv <*> randIndivs (pred n)

initial :: Randomizable a => Evo a (Population a)
initial = randIndivs =<< asks popSize

select :: Randomizable a => Population a -> Evo a (Indiv a)
select ts = do
  t1 <- randElem ts
  t2 <- randElem ts
  return $ if snd t1 <= snd t2 then t1 else t2

breed :: Randomizable a => Population a -> Evo a (Indiv a)
breed ts = do
  t1 <- fst <$> select ts
  t2 <- fst <$> select ts
  t' <- crossover t1 t2
  mkIndiv t'

insertIndiv :: Indiv a -> Population a -> Population a
insertIndiv t ts = insertBy (\x y -> compare (snd x) (snd y)) t ts

tooLarge :: Indiv a -> Evo a Bool
tooLarge t = (depth (fst t) >) <$> asks maxCrossDepth

isSolution :: Indiv a -> Bool
isSolution t = snd t == 0

mutateGen :: Randomizable a => Population a -> Evo a (Population a)
mutateGen = foldM (\xs x -> flip insertIndiv xs <$> mutateIndiv x) []

crossoverGen :: Randomizable a => Population a -> Population a -> Evo a (Population a)
crossoverGen ts ts' | length ts <= length ts' = return ts'
crossoverGen ts ts' | otherwise = do
  t' <- breed ts
  cond <- tooLarge t'
  if cond
  then crossoverGen ts ts'
  else crossoverGen ts (insertIndiv t' ts')

nextGen :: Randomizable a => Population a -> Population a -> Evo a (Population a)
nextGen ts ts' = crossoverGen ts ts'

elites :: Population a -> Evo a (Population a)
elites ts = do
  elitism <- asks elitism
  return $ take (truncate (fromIntegral (length ts) * elitism)) ts

evolve :: Randomizable a => Gen -> Population a -> Evo a (Gen , Population a)
evolve n ts = do
  maxGen <- asks maxGen
  if n >= maxGen || isSolution (head ts)
  then return (n , ts)
  else evolve (succ n) =<< nextGen ts =<< elites ts
  -- else evolve (succ n) =<< initial

evo :: Randomizable a => Evo a (Gen , Population a)
evo = evolve 0 =<< initial

runEvo :: Randomizable a => Options a -> [(Gen , Population a)]
runEvo opts = map (\r -> fst $ runState (runReaderT evo opts') r) rs
  where
  rs = map mkStdGen $ take (attempts opts) $ randoms (seed opts)
  opts' = opts { cases = map (bimap id norm) (cases opts) }

----------------------------------------------------------------------

defaultOpts :: Options a
defaultOpts = Options
  { name = "Untitled"
  , maxInitDepth = 20
  , maxCrossDepth = 17
  , popSize = 1000
  , maxGen = 30
  , elitism = 0.3
  , mutationRate = 0.0
  , minStruture = 15
  , attempts = 10
  , seed = mkStdGen 42
  , cases = []
  }

----------------------------------------------------------------------