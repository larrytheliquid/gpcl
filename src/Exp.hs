{-# LANGUAGE
    ViewPatterns
  #-}

module Exp where
import Tree

----------------------------------------------------------------------

data Comb = S | K
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

infixl 5 :@:
data Exp = Exp :@: Exp | Var String | Comb Comb
  deriving (Show,Read,Eq)

enum :: (Enum a, Bounded a) => [a]
enum = enumFromTo minBound maxBound

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

score :: Exp -> Int
score = undefined

err :: Tree a -> Int
err t = 7

----------------------------------------------------------------------