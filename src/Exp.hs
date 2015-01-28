{-# LANGUAGE
    ViewPatterns
  #-}

module Exp where
import Tree

----------------------------------------------------------------------

minStruture :: Int
minStruture = 10

----------------------------------------------------------------------

data Comb = S | K
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

infixl 5 :@:
data Exp = Exp :@: Exp | Var String | Comb Comb
  deriving (Show,Read,Eq)

enum :: (Enum a, Bounded a) => [a]
enum = enumFromTo minBound maxBound

nodes :: Exp -> Int
nodes (Var _) = 1
nodes (Comb _) = 1
nodes (x :@: y) = 1 + nodes x + nodes y

combs :: Exp -> Int
combs (Var _) = 0
combs (Comb _) = 1
combs (x :@: y) = nodes x + nodes y

----------------------------------------------------------------------

_S :: Exp
_S = Comb S

_K :: Exp
_K = Comb K

-- norm (_I :@: Var "x")
_I :: Exp
_I = _S :@: _K :@: _K

-- norm (_T :@: Var "x" :@: Var "f")
_T :: Exp
_T = _S :@: (_K :@: (_S :@: _I)) :@: _K

_T' :: Exp
_T' = _S :@: (_K :@: (_S :@: _I)) :@: (_S :@: (_K :@: _K) :@: _I)

-- would loop
-- norm ((_S :@: Var "x" :@: Var "y" :@: Var "z") :@: Var "a")

----------------------------------------------------------------------

toExp :: Tree Comb -> Exp
toExp (Branch l r) = toExp l :@: toExp r
toExp (Leaf c) = Comb c

rewrite :: Exp -> Exp
rewrite (Comb S :@: (rewrite -> x) :@: (rewrite -> y) :@: (rewrite -> z)) = x :@: z :@: (y :@: z)
rewrite (Comb K :@: (rewrite -> x) :@: _) = x
rewrite ((rewrite -> f) :@: (rewrite -> x)) = f :@: x
rewrite x = x

norm :: Exp -> Exp
norm x = if x' == x'' then x' else norm x''
  where
  x'  = rewrite x
  x'' = rewrite x'

----------------------------------------------------------------------

diff :: Exp -> Exp -> Int
diff (Var  x) (Var  y) | x == y = 0
diff (Comb x) (Comb y) | x == y = 0
diff (x1 :@: y1) (x2 :@: y2) = diff x1 x2 + diff y1 y2
diff x y = succ (abs (nodes x - nodes y))

-- TODO list of arguments for tree to be applied to
score :: Tree Comb -> Exp -> Int
score t e = diff lhs rhs * weight
  where
  lhs = toExp t
  rhs = norm e
  structure = leaves t
  weight = if structure >= minStruture then 1 else minStruture - succ structure

----------------------------------------------------------------------