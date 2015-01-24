module Evo where
import Control.Monad.State
import System.Random

----------------------------------------------------------------------

data Tree a = Branch (Tree a) (Tree a) | Leaf a

type Crumb a = Either (Tree a) (Tree a)
type Crumbs a = [Crumb a]

type Zipper a = (Tree a , Crumbs a)

----------------------------------------------------------------------

depth :: Tree a -> Int
depth (Leaf _) = 0
depth (Branch l r) = 1 + max (depth l) (depth r)

root :: Tree a -> Zipper a
root t = (t , [])

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

----------------------------------------------------------------------

data Comb = S | K | I
data Exp = App Exp Exp | Var String | Comb Comb

norm :: Exp -> Exp
norm = undefined

score :: Exp -> Int
score = undefined

----------------------------------------------------------------------

type RandM = State StdGen

----------------------------------------------------------------------


