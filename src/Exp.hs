module Exp where
import Tree
import Data.Maybe

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

_O :: Exp
_O = _S :@: _I :@: _I :@: (_S :@: _I :@: _I)

-- would loop
-- norm ((_S :@: Var "x" :@: Var "y" :@: Var "z") :@: Var "a")

----------------------------------------------------------------------

toExp :: Tree Comb -> Exp
toExp (Branch l r) = toExp l :@: toExp r
toExp (Leaf c) = Comb c

step :: Exp -> Maybe Exp
step (Comb S :@: x :@: y :@: z) = Just $ x :@: z :@: (y :@: z)
step (Comb K :@: x :@: _) = Just x
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
-- TODO check if succ is inside or outside minus
diff x y = succ (abs (nodes x - nodes y))

-- TODO list of arguments for tree to be applied to
score :: Tree Comb -> Exp -> Int
score t e = nodes (norm (toExp t))
-- score t e = diff lhs rhs * weight
--   where
--   lhs = toExp t
--   rhs = norm e
--   structure = leaves t
--   weight = if structure >= minStruture then 1 else minStruture - succ structure

----------------------------------------------------------------------