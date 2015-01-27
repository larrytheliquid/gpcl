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

----------------------------------------------------------------------

toExp :: Tree Comb -> Exp
toExp (Branch l r) = toExp l :@: toExp r
toExp (Leaf c) = Comb c

norm' :: Exp -> Exp
norm' (Comb S :@: x :@: y :@: z) = norm' (x :@: z :@: (y :@: z))
norm' (Comb K :@: x :@: _) = norm' x
norm' (f :@:  x) = norm' f :@: norm' x
norm' x = x

norm :: Exp -> Exp
norm x = if x' == x'' then x' else norm x''
  where
  x'  = norm' x
  x'' = norm' x'

----------------------------------------------------------------------

score :: Exp -> Int
score = undefined

err :: Tree a -> Int
err t = 7

----------------------------------------------------------------------