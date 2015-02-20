{-# LANGUAGE
    OverloadedStrings
  #-}


module Exp where
import Tree
import Data.Maybe
import Data.String

----------------------------------------------------------------------

minStruture :: Int
minStruture = 15

----------------------------------------------------------------------

data Comb = S | K | I | C | B
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

infixl 5 :@:
data Exp = Exp :@: Exp | Var String | Comb Comb
  deriving (Show,Read,Eq)

----------------------------------------------------------------------

instance IsString Exp where
  fromString = Var

----------------------------------------------------------------------

enum :: (Enum a, Bounded a) => [a]
enum = enumFromTo minBound maxBound

----------------------------------------------------------------------

nodes :: Exp -> Int
nodes (Var _) = 1
nodes (Comb _) = 1
nodes (x :@: y) = 1 + nodes x + nodes y

combs :: Exp -> Int
combs (Var _) = 0
combs (Comb _) = 1
combs (x :@: y) = nodes x + nodes y

----------------------------------------------------------------------

type Args = [String]
apply :: Exp -> Args -> Exp
apply = foldl (\f x -> f :@: Var x)

----------------------------------------------------------------------

_S :: Exp
_S = Comb S

_K :: Exp
_K = Comb K

-- norm (_I :@: Var "x")
_I :: Exp
_I = Comb I
-- _I = _S :@: _K :@: _K

_B :: Exp
_B = Comb B

_C :: Exp
_C = Comb C

-- norm (_T :@: Var "x" :@: Var "f")
_T :: Exp
-- _T = _S :@: (_K :@: (_S :@: _I)) :@: _K
_T = _C :@: _I

_T' :: Exp
_T' = _S :@: (_K :@: (_S :@: _I)) :@: (_S :@: (_K :@: _K) :@: _I)

_O :: Exp
_O = _S :@: _I :@: _I :@: (_S :@: _I :@: _I)

-- would loop
-- norm ((_S :@: Var "x" :@: Var "y" :@: Var "z") :@: Var "a")

_true :: Exp
_true = _K

_false :: Exp
_false = _K :@: _I

_pair :: Exp
_pair = _B :@: _C :@: _T

_first :: Exp
_first = _T :@: _true

_second :: Exp
_second = _T :@: _false

----------------------------------------------------------------------

toExp :: Tree Comb -> Exp
toExp (Branch l r) = toExp l :@: toExp r
toExp (Leaf c) = Comb c

fromExp :: Exp -> Tree Comb
fromExp (l :@: r) = Branch (fromExp l) (fromExp r)
fromExp (Comb c) = Leaf c
fromExp (Var _) = error "Variables not allowed"

step :: Exp -> Maybe Exp
step (Comb S :@: x :@: y :@: z) = Just $ x :@: z :@: (y :@: z)
step (Comb K :@: x :@: _) = Just x
step (Comb I :@: x) = Just x
step (Comb C :@: x :@: y :@: z) = Just $ x :@: z :@: y
step (Comb B :@: x :@: y :@: z) = Just $ x :@: (y :@: z)
step x@(Var _) = Just x
step x@(Comb _) = Just x
step (x :@: y) = case (step x , step y) of
  (Just x' , Just y') -> Just (x' :@: y')
  (Just x' , Nothing) -> Just (x' :@: y)
  (Nothing , Just y') -> Just (x :@: y')
  (Nothing , Nothing) -> Nothing

stepTo :: Int -> Exp -> Exp
stepTo 0 x = x
stepTo n x = maybe x (stepTo (pred n)) (step x)

norm :: Exp -> Exp
norm = stepTo 20

----------------------------------------------------------------------

diff :: Exp -> Exp -> Int
diff (Var  x) (Var  y) | x == y = 0
diff (Comb x) (Comb y) | x == y = 0
diff (x1 :@: y1) (x2 :@: y2) = diff x1 x2 + diff y1 y2
diff x y = succ (abs (nodes x - nodes y))

score :: Tree Comb -> Args -> Exp -> (Tree Comb , Int)
score t args e2 = (t' , diff lhs rhs * weight)
  where
  e1 = toExp t
  t' = fromExp (norm e1)
  lhs = norm (e1 `apply` args)
  rhs = norm e2
  structure = leaves t
  weight = if structure >= minStruture then 1 else minStruture - structure + 1

----------------------------------------------------------------------