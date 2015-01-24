module Evo where
import Control.Monad.State
import System.Random

----------------------------------------------------------------------

data Tree a = Branch (Tree a) (Tree a) | Leaf a
  deriving (Show,Read,Eq)

type Crumb a = Either (Tree a) (Tree a)
type Crumbs a = [Crumb a]

type Zipper a = (Tree a , Crumbs a)

----------------------------------------------------------------------

depth :: Tree a -> Int
depth (Leaf _) = 0
depth (Branch l r) = 1 + max (depth l) (depth r)

size :: Tree a -> Int
size (Leaf _) = 1
size (Branch l r) = 1 + size l + size r

maxIndex :: Tree a -> Int
maxIndex = pred . size

root :: Tree a -> Zipper a
root t = (t , [])

isRoot :: Zipper a -> Bool
isRoot (t , []) = True
isRoot _ = False

isLeaf :: Zipper a -> Bool
isLeaf (Leaf _ , _) = True
isLeaf _ = False

goUp :: Zipper a -> Zipper a
goUp (l , Left r : xs) = (Branch l r , xs)
goUp (r , Right l : xs) = (Branch l r , xs)
goUp _ = error "Cannot go Up"

goLeft :: Zipper a -> Zipper a
goLeft (Branch l r , xs) = (l , Left r : xs)
goLeft _ = error "Cannot go left"

goRight :: Zipper a -> Zipper a
goRight (Branch l r , xs) = (r , Right l : xs)
goRight _ = error "Cannot go right"

traverse :: Zipper a -> [Zipper a]
traverse z | isLeaf z = [z]
traverse z | otherwise = z : (traverse (goLeft z) ++ traverse (goRight z))

locate :: Tree a -> Int -> Zipper a
locate t i = traverse (root t) !! i

attach :: Tree a -> Zipper a -> Zipper a
attach t (_ , xs) = (t , xs)

goRoot :: Zipper a -> Zipper a
goRoot z | isRoot z = z
goRoot z | otherwise = goRoot (goUp z)

rootTree :: Zipper a -> Tree a
rootTree = fst . goRoot

----------------------------------------------------------------------

data Comb = S | K | I
  deriving (Show,Read,Eq,Ord,Enum,Bounded)
data Exp = App Exp Exp | Var String | Comb Comb

cardComb :: Int
cardComb = succ (fromEnum (maxBound :: Comb))

norm :: Exp -> Exp
norm = undefined

score :: Exp -> Int
score = undefined

----------------------------------------------------------------------

type RandM = State StdGen

rand :: Int -> RandM Int
rand bound = do
  s <- get
  let (r , s') = randomR (0, bound) s
  put s'
  return r

crossover :: (Tree a , Tree a) -> RandM (Tree a , Tree a)
crossover (t1 , t2) = do
  i1 <- rand (maxIndex t1)
  i2 <- rand (maxIndex t2)
  let h1 = locate t1 i1
  let h2 = locate t2 i2
  return (rootTree (attach t2 h1) , rootTree (attach t1 h2))

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


