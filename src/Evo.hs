{-# LANGUAGE
    ViewPatterns
  , OverloadedStrings
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
maxInitDepth = 10

maxCrossDepth :: Int
maxCrossDepth = 17

popSize :: Int
popSize = 1000

maxGen :: Int
maxGen = 30

elitism :: Float
elitism = 0.3

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
evolve n ts | otherwise = evolve (succ n) =<< nextGen ts elite
  where
  -- elite = take 1 ts
  elite = take (truncate (fromIntegral (length ts) * elitism)) ts

evo :: Evo (Gen , Population)
evo = evolve 0 =<< initial

runEvo :: Exp -> Args -> Int -> (Gen , Population)
runEvo e args i = fst $ runState (runReaderT evo (e , args)) (mkStdGen i)

infix 2 #
(#) :: String -> Exp -> (Gen , [Int])
args # e = bimap id (map snd) $ runEvo e (map show args) 199

----------------------------------------------------------------------

-- http://www.angelfire.com/tx4/cus/combinator/birds.html

-- solved
_K  = "ab"   # "a"
_I  = "a"    # "a"
_T  = "ab"   # "b" :@: "a"
_B  = "abc"  # "a" :@: ("b" :@: "c")
_W  = "ab"   # "a" :@: ("b" :@: "b")
             
_M  = "a"    # "a" :@: "a"
_M2 = "ab"   # "a" :@: "b" :@: ("a" :@: "b")
_D  = "abcd" # "a" :@: "b" :@: ("c" :@: "d")
_Q3 = "abc"  # "c" :@: ("a" :@: "b")

-- unsolved
_C  = "abc"  # "a" :@: "c" :@: "b"
_B1 = "abcd" # "a" :@: ("b" :@: "c" :@: "d")
_Q  = "abc"  # "b" :@: ("a" :@: "c")
_U  = "ab"   # "b" :@: ("a" :@: "a" :@: "b")

----------------------------------------------------------------------


