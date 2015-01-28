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

maxInitDepth :: Int
maxInitDepth = 10

maxCrossDepth :: Int
maxCrossDepth = 17

popSize :: Int
popSize = 1000

maxGen :: Int
maxGen = 50

----------------------------------------------------------------------

type Rand = State StdGen
type Gen = Int

randInt :: Int -> Rand Int
randInt n = do
  s <- get
  let (r , s') = randomR (0, pred n) s
  put s'
  return r

randBool :: Rand Bool
randBool = (0 ==) <$> randInt 2

randElem :: [a] -> Rand a
randElem xs = (xs !!) <$> randInt (length xs)

randZip :: Tree a -> Rand (Zipper a)
randZip t = locate t <$> randInt (size t)

----------------------------------------------------------------------

crossover :: Tree a -> Tree a -> Rand (Tree a)
crossover t1 t2 = do
  z1 <- randZip t1
  z2 <- randZip t2
  return $ rootTree (replace (currentTree z2) z1)

randTree' :: (Enum a, Bounded a) => Int -> Rand (Tree a)
randTree' n = do
  b <- randBool
  if b || n <= 0
  then Leaf <$> randElem enum
  else Branch <$> randTree' (pred n) <*> randTree' (pred n)

randTree :: (Enum a, Bounded a) => Rand (Tree a)
randTree = randTree' maxInitDepth

-- map depth (fst (runState (replicateM 5 (randTree :: Rand (Tree Comb))) (mkStdGen 7)))
-- [1,10,0,2,2]

----------------------------------------------------------------------

type Target = Exp
type Indiv = (Tree Comb , Int)
type Population = [Indiv]

randIndiv :: Rand Indiv
randIndiv = do
  t <- randTree
  return (t , err t)

initial :: Rand Population
initial = replicateM popSize randIndiv

select :: Population -> Rand Indiv
select ts = do
  t1 <- randElem ts
  t2 <- randElem ts
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

-- map (depth . fst) (snd (fst (runState gp (mkStdGen 199))))
-- (depth . fst . head) (snd (fst (runState gp (mkStdGen 199))))

----------------------------------------------------------------------


