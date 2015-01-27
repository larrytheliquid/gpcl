{-# LANGUAGE
    ViewPatterns
  #-}

module Exp where
import Tree

----------------------------------------------------------------------

data Comb = S | K | I
  deriving (Show,Read,Eq,Ord,Enum,Bounded)
data Exp = App Exp Exp | Var String | Comb Comb

enum :: (Enum a, Bounded a) => [a]
enum = enumFromTo minBound maxBound

toExp :: Tree Comb -> Exp
toExp = undefined

norm :: Exp -> Exp
norm = undefined

score :: Exp -> Int
score = undefined

err :: Tree a -> Int
err t = 7

----------------------------------------------------------------------