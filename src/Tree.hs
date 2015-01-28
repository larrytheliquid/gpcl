module Tree where

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

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Branch l r) = leaves l + leaves r

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

replace :: Tree a -> Zipper a -> Zipper a
replace t (_ , xs) = (t , xs)

goRoot :: Zipper a -> Zipper a
goRoot z | isRoot z = z
goRoot z | otherwise = goRoot (goUp z)

currentTree :: Zipper a -> Tree a
currentTree = fst

rootTree :: Zipper a -> Tree a
rootTree = currentTree . goRoot

eg :: Tree Char
eg = Branch (Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

----------------------------------------------------------------------