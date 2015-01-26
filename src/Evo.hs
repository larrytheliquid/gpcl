{-# LANGUAGE
    ViewPatterns
  #-}

module Evo where
import Tree
import Exp
import Control.Applicative
import Control.Monad.State
import System.Random

----------------------------------------------------------------------

probA :: Double
probA = 0.5

probC :: Double
probC = 0.5 / fromIntegral cardComb

maxInitDepth :: Int
maxInitDepth = 10

maxCrossDepth :: Int
maxCrossDepth = 17

minStruture :: Int
minStruture = 10

popSize :: Int
popSize = 1000

maxGen :: Int
maxGen = 50

eg :: Tree Char
eg = Branch (Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

----------------------------------------------------------------------

type Rand = State StdGen
type Gen = Int

rand :: Int -> Rand Int
rand bound = do
  s <- get
  let (r , s') = randomR (0, pred bound) s
  put s'
  return r

crossover :: Tree a -> Tree a -> Rand (Tree a)
crossover t1 t2 = do
  z1 <- locate t1 <$> rand (size t1)
  z2 <- locate t2 <$> rand (size t2)
  return $ rootTree (replace (currentTree z2) z1)

type Indiv = Tree Comb
type Population = [Indiv]

randIndiv :: Rand Indiv
randIndiv = undefined

initial :: Rand Population
initial = undefined

select :: Population -> Rand Indiv
select ts = do
  t1 <- (ts !!) <$> rand (length ts)
  t2 <- (ts !!) <$> rand (length ts)
  return $ if err t1 <= err t2 then t1 else t2

breed :: Population -> Rand Indiv
breed ts = do
  t1 <- select ts
  t2 <- select ts
  crossover t1 t2

nextGen :: Population -> Population -> Rand Population
nextGen ts ts' | length ts <= length ts' = return ts'
nextGen ts ts' | otherwise = do
  t' <- breed ts
  if depth t' > maxCrossDepth
  then nextGen ts ts'
  else nextGen ts (t' : ts')

evolve :: Gen -> Population -> Rand Population
evolve n ts | n >= maxGen = return ts
evolve n ts | otherwise = evolve (succ n) =<< nextGen ts []

gp :: Rand Population
gp = evolve 0 =<< initial

----------------------------------------------------------------------


