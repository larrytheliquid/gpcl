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

numGens :: Int
numGens = 50

eg :: Tree Char
eg = Branch (Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

----------------------------------------------------------------------

type Rand = State StdGen

rand :: Int -> Rand Int
rand bound = do
  s <- get
  let (r , s') = randomR (0, pred bound) s
  put s'
  return r

crossover :: (Tree a , Tree a) -> Rand (Tree a , Tree a)
crossover (t1 , t2) = do
  z1 <- locate t1 <$> rand (size t1)
  z2 <- locate t2 <$> rand (size t2)
  return ( rootTree (replace (currentTree z2) z1)
         , rootTree (replace (currentTree z1) z2) )

-- TODO use maxCrossDepth

type Indiv = Tree Comb
type Population = [Indiv]

select :: Population -> Rand (Tree Indiv)
select ts = do
  t1 <- (ts !!) <$> rand (length ts)
  t2 <- (ts !!) <$> rand (length ts)
  undefined

----------------------------------------------------------------------


