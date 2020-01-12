{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin


module Instances where

import Prelude hiding (reverse)

import Data.Char (isSpace)
import Data.Function (on)

newtype Pointwise a b = Pointwise {getPointwise :: (a, b)}
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (Pointwise a b) where
  (<=) :: Pointwise a b -> Pointwise a b -> Bool
  Pointwise (a, b) <= Pointwise (c, d) = (a <= c) && ( b <= d)

newtype Lexicographic a b = Lexicographic {getLexicographic :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples and lists
instance (Ord a, Ord b) => Ord (Lexicographic a b) where
  (<=) :: Lexicographic a b -> Lexicographic a b -> Bool
  Lexicographic (a, b) <= Lexicographic (c, d) = (a < c) || ((a <= c) && (b <= d))

newtype Fun a b = Fun {getFun :: a -> b}

instance (Semigroup b) => Semigroup (Fun a b) where
  (<>) :: Fun a b -> Fun a b -> Fun a b
  (Fun x) <> (Fun y) = Fun (\u -> x u <> y u) 

instance (Monoid b) => Monoid (Fun a b) where
  mempty :: Fun a b
  mempty = Fun $ mempty getFun 

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) :: First a -> First a -> First a
  First (Just a)  <> _ = First $ Just a
  First Nothing <> a   = a  


instance Monoid (First a) where
  mempty :: First a
  mempty = First Nothing 

newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) :: Last a -> Last a -> Last a
  _ <> Last (Just x) = Last $ Just x   
  a <> Last Nothing  = a 

instance Monoid (Last a) where
  mempty :: Last a
  mempty = Last Nothing

newtype Pair a b = Pair {getPair :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples
instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) :: Pair a b -> Pair a b -> Pair a b
  Pair (x, y) <> Pair (u, v) = Pair (x <> u ,y <> v)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty :: Pair a b
  mempty = Pair (mempty, mempty)

newtype Dual a = Dual {getDual :: a}
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Dual a) where
  (<>) :: Dual a -> Dual a -> Dual a
  Dual x <> Dual y = Dual $ y <> x  

instance Monoid a => Monoid (Dual a) where
  mempty :: Dual a
  mempty = Dual mempty

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = getDual $ Dual [x] <> Dual (reverse xs) 

--reverse xs = getDual $ foldMap <> (Dual xs)
--reverse xs = getDual (foldr (\x rec -> (Dual [x]) <> (Dual rec))  mempty xs))


data Flux a = Flux
  { sides :: Maybe (a, a)
  , changes :: Int
  }
  deriving (Show, Eq)

flux :: a -> Flux a
flux x = Flux (Just (x, x)) 0

instance (Eq a) => Semigroup (Flux a) where
  (<>) :: Flux a -> Flux a -> Flux a
  Flux Nothing _ <> a = a 
  a <> Flux Nothing _= a 
  Flux (Just (x1,x2 )) y <> Flux (Just (z1,z2)) u 
   | x2 == z1 = Flux (Just (x1, z2)) (y + u)
   | otherwise = Flux (Just (x1, z2)) (y + u + 1)
   

instance (Eq a) => Monoid (Flux a) where
  mempty :: Flux a
  mempty = Flux Nothing 0 
