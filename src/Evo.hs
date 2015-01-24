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
traverse t | isLeaf t = [t]
traverse t | otherwise = t : (traverse (goLeft t) ++ traverse (goRight t))

getNode :: Tree a -> Int -> Zipper a
getNode t i = traverse (root t) !! i

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


