module Magma where

import Data.Monoid
import Data.Foldable

class Magma a where
  magappend :: a -> a -> a

data FreeMagma a = Leaf a | Node (FreeMagma a) (FreeMagma a) deriving (Show, Eq)

foldMapMagma :: Magma b => (a -> b) -> FreeMagma a -> b
foldMapMagma f (Leaf x) = f x
foldMapMagma f (Node l r) = foldMapMagma f l `magappend` foldMapMagma f r

instance Magma (FreeMagma a) where
  magappend = Node

newtype WrappedMonoid a = WrapMonoid a deriving (Show, Eq)
instance Monoid a => Magma (WrappedMonoid a) where
  WrapMonoid x `magappend` WrapMonoid y = WrapMonoid (x `mappend` y)

newtype Kakko = Kakko String deriving (Show)
instance Magma Kakko where
  Kakko x `magappend` Kakko y = Kakko ("(" ++ x ++ " " ++ y ++ ")")
