{-# LANGUAGE
    OverloadedStrings
  , ViewPatterns
  , ConstraintKinds
  #-}


module Exp where
import Tree
import Data.Maybe
import Data.String

----------------------------------------------------------------------

minStruture :: Int
minStruture = 15

----------------------------------------------------------------------

data SK = S' | K'
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

data Comb = S | K | I | C | B
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

infixl 5 :@:
data Exp a = Exp a :@: Exp a | Var String | Prim a
  deriving (Show,Read,Eq)

class Contractible a where
  contract :: Exp a -> Maybe (Exp a)

instance Contractible SK where
  contract (Prim S' :@: x :@: y :@: z) = Just $ x :@: z :@: (y :@: z)
  contract (Prim K' :@: x :@: _) = Just x
  contract _ = Nothing

instance Contractible Comb where
  contract (Prim S :@: x :@: y :@: z) = Just $ x :@: z :@: (y :@: z)
  contract (Prim K :@: x :@: _) = Just x
  contract (Prim I :@: x) = Just x
  contract (Prim C :@: x :@: y :@: z) = Just $ x :@: z :@: y
  contract (Prim B :@: x :@: y :@: z) = Just $ x :@: (y :@: z)
  contract _ = Nothing

----------------------------------------------------------------------

instance IsString (Exp a) where
  fromString = Var

----------------------------------------------------------------------

type Enumerable a = (Enum a, Bounded a)

enum :: Enumerable a => [a]
enum = enumFromTo minBound maxBound

----------------------------------------------------------------------

nodes :: Exp a -> Int
nodes (Var _) = 1
nodes (Prim _) = 1
nodes (x :@: y) = 1 + nodes x + nodes y

combs :: Exp a -> Int
combs (Var _) = 0
combs (Prim _) = 1
combs (x :@: y) = nodes x + nodes y

----------------------------------------------------------------------

type Args = [String]
apply :: Exp a -> Args -> Exp a
apply = foldl (\f x -> f :@: Var x)

----------------------------------------------------------------------

_S :: Exp Comb
_S = Prim S

_K :: Exp Comb
_K = Prim K

-- norm (_I :@: Var "x")
_I :: Exp Comb
_I = Prim I
-- _I = _S :@: _K :@: _K

_B :: Exp Comb
_B = Prim B

_C :: Exp Comb
_C = Prim C

-- norm (_T :@: Var "x" :@: Var "f")
_T :: Exp Comb
-- _T = _S :@: (_K :@: (_S :@: _I)) :@: _K
_T = _C :@: _I

_T' :: Exp Comb
_T' = _S :@: (_K :@: (_S :@: _I)) :@: (_S :@: (_K :@: _K) :@: _I)

_O :: Exp Comb
_O = _S :@: _I :@: _I :@: (_S :@: _I :@: _I)

-- would loop
-- norm ((_S :@: Var "x" :@: Var "y" :@: Var "z") :@: Var "a")

_true :: Exp Comb
_true = _K

_false :: Exp Comb
_false = _K :@: _I

_pair :: Exp Comb
_pair = _B :@: _C :@: _T

_first :: Exp Comb
_first = _T :@: _true

_second :: Exp Comb
_second = _T :@: _false

----------------------------------------------------------------------

toExp :: Tree a -> Exp a
toExp (Branch l r) = toExp l :@: toExp r
toExp (Leaf c) = Prim c

fromExp :: Exp a -> Tree a
fromExp (l :@: r) = Branch (fromExp l) (fromExp r)
fromExp (Prim c) = Leaf c
fromExp (Var _) = error "Variables not allowed"

step :: Contractible a => Exp a -> Maybe (Exp a)
step x@(Var _) = Just x
step x@(Prim _) = Just x
step (contract -> Just x) = Just x
step (x :@: y) = case (step x , step y) of
  (Just x' , Just y') -> Just (x' :@: y')
  (Just x' , Nothing) -> Just (x' :@: y)
  (Nothing , Just y') -> Just (x :@: y')
  (Nothing , Nothing) -> Nothing

stepTo :: Contractible a => Int -> Exp a -> Exp a
stepTo 0 x = x
stepTo n x = maybe x (stepTo (pred n)) (step x)

norm :: Contractible a => Exp a -> Exp a
norm = stepTo 20

----------------------------------------------------------------------

diff :: Eq a => Exp a -> Exp a -> Int
diff (Var  x) (Var  y) | x == y = 0
diff (Prim x) (Prim y) | x == y = 0
diff (x1 :@: y1) (x2 :@: y2) = diff x1 x2 + diff y1 y2
diff x y = succ (abs (nodes x - nodes y))

type Scorable a = (Eq a, Contractible a)

score :: Scorable a => Tree a -> Args -> Exp a -> (Tree a , Int)
score t args e2 = (t' , diff lhs rhs * weight)
  where
  e1 = toExp t
  t' = fromExp (norm e1)
  lhs = norm (e1 `apply` args)
  rhs = norm e2
  structure = leaves t
  weight = if structure >= minStruture then 1 else minStruture - structure + 1

----------------------------------------------------------------------