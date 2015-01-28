{-# LANGUAGE
    ViewPatterns
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

maxInitDepth :: Int
maxInitDepth = 20

maxCrossDepth :: Int
maxCrossDepth = 27

popSize :: Int
popSize = 1000

maxGen :: Int
maxGen = 50

----------------------------------------------------------------------

type Evo = ReaderT (Exp , Args) (State StdGen)
type Gen = Int

randInt :: Int -> Evo Int
randInt n = do
  s <- get
  let (r , s') = randomR (0, pred n) s
  put s'
  return r

randBool :: Evo Bool
randBool = (0 ==) <$> randInt 2

randElem :: [a] -> Evo a
randElem xs = (xs !!) <$> randInt (length xs)

randZip :: Tree a -> Evo (Zipper a)
randZip t = locate t <$> randInt (size t)

mkIndiv :: Tree Comb -> Evo Indiv
mkIndiv t = do
  (e , args) <- ask
  return (t , score t args e)

----------------------------------------------------------------------

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

initial :: Evo Population
initial = replicateM popSize randIndiv

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

nextGen :: Population -> Population -> Evo Population
nextGen ts ts' | length ts <= length ts' = return ts'
nextGen ts ts' | otherwise = do
  t' <- breed ts
  if tooLarge t'
  then nextGen ts ts'
  else nextGen ts (insertIndiv t' ts')

evolve :: Gen -> Population -> Evo (Gen , Population)
evolve n ts | n >= maxGen || isSolution (head ts) = return (n , ts)
evolve n ts | otherwise = evolve (succ n) =<< nextGen ts [head ts]

evo :: Evo (Gen , Population)
evo = evolve 0 =<< initial

runEvo :: Exp -> Args -> Int -> (Gen , Population)
runEvo e args i = fst $ runState (runReaderT evo (e , args)) (mkStdGen i)

-- bimap id (map snd) $ runEvo _K ["a", "b"] 199
-- bimap id (map snd) $ runEvo _I ["a"] 199
-- bimap id (map snd) $ runEvo _T ["x" , "f"] 199

-- http://www.angelfire.com/tx4/cus/combinator/birds.html

-- solved
-- _K -- bimap id (map snd) $ runEvo (Var "x") ["x", "y"] 199
-- _I -- bimap id (map snd) $ runEvo (Var "x") ["x"] 199
-- _T -- bimap id (map snd) $ runEvo (Var "f" :@: Var "x") ["x", "f"] 199
-- _B -- bimap id (map snd) $ runEvo (Var "f" :@: (Var "g" :@: Var "x")) ["f", "g", "x"] 199
-- _W -- bimap id (map snd) $ runEvo (Var "f" :@: Var "x" :@: Var "x") ["f", "x"] 199

-- unsolved
-- _C -- bimap id (map snd) $ runEvo (Var "f" :@: Var "y" :@: Var "x") ["f", "x", "y"] 199
-- _B1 -- bimap id (map snd) $ runEvo (Var "a" :@: (Var "b" :@: Var "c" :@: Var "d")) ["a", "b", "c", "d"] 199
-- _Q -- bimap id (map snd) $ runEvo (Var "b" :@: (Var "a" :@: Var "c")) ["a", "b", "c"] 199
-- _Q3 -- bimap id (map snd) $ runEvo (Var "c" :@: (Var "a" :@: Var "b")) ["a", "b", "c"] 199

-- solved at init=20, cross=27, struct=20
-- _M -- bimap id (map snd) $ runEvo (Var "a" :@: Var "a") ["a"] 199
-- _M2 -- bimap id (map snd) $ runEvo (Var "a" :@: Var "b" :@: (Var "a" :@: Var "b")) ["a", "b"] 199
-- _D -- bimap id (map snd) $ runEvo (Var "a" :@: Var "b" :@: (Var "c" :@: Var "d")) ["a", "b", "c", "d"] 199
      -- bimap id (map (size . fst)) $ runEvo (Var "a" :@: Var "b" :@: (Var "c" :@: Var "d")) ["a", "b", "c", "d"] 199



-- map (nodes . norm . toExp . fst) . snd $ runEvo _K 19
-- sort $ map (flip score' _K . fst) . snd $ runEvo _K 199
-- sort $ map (depth . fst) . snd $ runEvo _K 199
-- (depth . fst . head) . snd $ runEvo _K 199

----------------------------------------------------------------------


