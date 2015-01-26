{-# LANGUAGE
    ViewPatterns
  #-}

module Evo where
import Tree
import Exp
import Control.Applicative
import Control.Monad.State
import System.Random
import Data.List

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

randInt :: Int -> Rand Int
randInt bound = do
  s <- get
  let (r , s') = randomR (0, pred bound) s
  put s'
  return r

crossover :: Tree a -> Tree a -> Rand (Tree a)
crossover t1 t2 = do
  z1 <- locate t1 <$> randInt (size t1)
  z2 <- locate t2 <$> randInt (size t2)
  return $ rootTree (replace (currentTree z2) z1)

type Indiv = (Tree Comb , Int)
type Population = [Indiv]

randIndiv :: Rand Indiv
randIndiv = undefined

initial :: Rand Population
initial = undefined

select :: Population -> Rand Indiv
select ts = do
  t1 <- (ts !!) <$> randInt (length ts)
  t2 <- (ts !!) <$> randInt (length ts)
  return $ if snd t1 <= snd t2 then t1 else t2

breed :: Population -> Rand Indiv
breed ts = do
  t1 <- fst <$> select ts
  t2 <- fst <$> select ts
  t' <- crossover t1 t2
  return (t' , err t')

insertIndiv :: Indiv -> Population -> Population
insertIndiv t ts = insertBy (\x y -> compare (snd x) (snd y)) t ts

tooLarge :: Indiv -> Bool
tooLarge t = depth (fst t) > maxCrossDepth

isSolution :: Indiv -> Bool
isSolution t = snd t == 0

nextGen :: Population -> Population -> Rand Population
nextGen ts ts' | length ts <= length ts' = return ts'
nextGen ts ts' | otherwise = do
  t' <- breed ts
  if tooLarge t'
  then nextGen ts ts'
  else nextGen ts (insertIndiv t' ts')

evolve :: Gen -> Population -> Rand (Gen , Population)
evolve n ts | n >= maxGen || isSolution (head ts) = return (n , ts)
evolve n ts | otherwise = evolve (succ n) =<< nextGen ts [head ts]

gp :: Rand (Gen , Population)
gp = evolve 0 =<< initial

----------------------------------------------------------------------


