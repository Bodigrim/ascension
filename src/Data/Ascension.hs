{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Ascension 
  ( Ascending
  , fromDistinctAscList
  , toDistinctAscList
  , forceUntil
  , difference
  , intersection
  , filter 
  , partition
  , uncons
  , mapMonotonic
  , member
  ) where

import Prelude hiding (span, filter)
import qualified Data.List as L

import Data.Data
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Exts

data Limit a = Bottom | Middle a | Top

-- Invariant: maximal element of set is strictly below first element of list.
-- Invariant: list elements are strictly ascending (and consequently distinct).
-- TODO: keep limit of forced elements
data Ascending a = Ascending (Set a) {- (Limit a) -} [a]
  deriving (Show, Read, Foldable, Data)

instance Ord a => Eq (Ascending a) where 
  a1 == a2 = toList a1 == toList a2 

instance Ord a => Ord (Ascending a) where 
  a1 `compare` a2 = toList a1 `compare` toList a2

instance Ord a => IsList (Ascending a) where
  type (Item (Ascending a)) = a

  -- | Strict. See 'fromDistinctAscList' for lazy construction.
  fromList xs = Ascending (S.fromList xs) []

  toList = toDistinctAscList

-- Lazy. See 'fromList' for strict construction.
fromDistinctAscList :: [a] -> Ascending a
fromDistinctAscList = Ascending S.empty

toDistinctAscList :: Ascending a -> [a]
toDistinctAscList (Ascending s l) = S.toAscList s ++ l

forceUntil :: Ord a => a -> Ascending a -> Ascending a
forceUntil x a@(Ascending s l) = case S.lookupMax s of 
  Just y  -> if x <= y then a else Ascending (s <> S.fromDistinctAscList m) n
  Nothing -> Ascending (S.fromDistinctAscList m) n
  where
    -- TODO: improve limit of forced elements by first element of n 
    (m, n) = L.span (<= x) l

forceBoth :: Ord a => Ascending a -> Ascending a -> (Ascending a, Ascending a)
forceBoth a1@(Ascending s1 _) a2@(Ascending s2 _) = 
  -- TODO: new force boundary if max(lookupMax s1, lookupMax s2, min(force limit 1, force limit 2))
  case (S.lookupMax s1, S.lookupMax s2) of 
    (Nothing, Nothing) -> (a1, a2)
    (Just x, Nothing)  -> (a1, forceUntil x a2)
    (Nothing, Just y)  -> (forceUntil y a1, a2)
    (Just x, Just y)   -> case x `compare` y of 
      LT -> (forceUntil y a1, a2)
      EQ -> (a1, a2) 
      GT -> (a1, forceUntil x a2)

mergeAscLists :: Ord a => [a] -> [a] -> [a]
mergeAscLists xs [] = xs 
mergeAscLists [] ys = ys 
mergeAscLists xs@(x : xs') ys@(y : ys') = case x `compare` y of 
  LT -> x : mergeAscLists xs' ys 
  EQ -> x : mergeAscLists xs' ys' 
  GT -> y : mergeAscLists xs  ys' 

intersectAscLists :: Ord a => [a] -> [a] -> [a]
intersectAscLists _ [] = [] 
intersectAscLists [] _ = [] 
intersectAscLists xs@(x : xs') ys@(y : ys') = case x `compare` y of
  LT -> intersectAscLists xs' ys 
  EQ -> x : intersectAscLists xs' ys' 
  GT -> intersectAscLists xs ys' 

differenceAscLists :: Ord a => [a] -> [a] -> [a]
differenceAscLists xs [] = xs 
differenceAscLists [] _  = [] 
differenceAscLists xs@(x : xs') ys@(y : ys') = case x `compare` y of 
  LT -> x : differenceAscLists xs' ys 
  EQ -> differenceAscLists xs' ys' 
  GT -> differenceAscLists xs ys' 

memberAscList :: Ord a => a -> [a] -> Bool 
memberAscList x = \case 
  [] -> False 
  y : ys -> case x `compare` y of 
    LT -> False 
    EQ -> True 
    GT -> memberAscList x ys 

instance Ord a => Semigroup (Ascending a) where
  a1 <> a2 = let (Ascending s1 l1, Ascending s2 l2) = forceBoth a1 a2 in 
    Ascending (s1 <> s2) (mergeAscLists l1 l2)

instance Ord a => Monoid (Ascending a) where
  mempty  = Ascending mempty mempty 
  mappend = (<>)

filter :: (a -> Bool) -> Ascending a -> Ascending a
filter = (fst .) . partition

difference :: Ord a => Ascending a -> Ascending a -> Ascending a
difference a1 a2 = let (Ascending s1 l1, Ascending s2 l2) = forceBoth a1 a2 in 
  Ascending (s1 `S.difference` s2) (differenceAscLists l1 l2)

intersection :: Ord a => Ascending a -> Ascending a -> Ascending a
intersection a1 a2 = let (Ascending s1 l1, Ascending s2 l2) = forceBoth a1 a2 in 
  Ascending (s1 `S.intersection` s2) (intersectAscLists l1 l2)

uncons :: Ascending a -> Maybe (a, Ascending a)
uncons (Ascending s l) = case S.minView s of 
  Nothing -> case l of 
    []     -> Nothing 
    x : xs -> Just (x, Ascending S.empty xs)
  Just (x, s') -> Just (x, Ascending s' l)

mapMonotonic :: (a -> b) -> Ascending a -> Ascending b
mapMonotonic f (Ascending s l) = Ascending (S.mapMonotonic f s) (map f l)

member :: Ord a => a -> Ascending a -> Bool
member x (Ascending s l) = case S.lookupMax s of 
  Nothing -> memberAscList x l 
  Just y  -> case x `compare` y of 
    LT -> S.member x s 
    EQ -> True 
    GT -> memberAscList x l  

splitAt :: Int -> Ascending a -> Ascending a
splitAt = undefined

span :: (a -> Bool) -> Ascending a -> (Ascending a, Ascending a)
span = undefined

partition :: (a -> Bool) -> Ascending a -> (Ascending a, Ascending a)
partition predicate (Ascending s l) = (Ascending s1 l1, Ascending s2 l2)
  where 
    (s1, s2) = S.partition predicate s 
    (l1, l2) = L.partition predicate l 

elemAt :: Int -> Ascending a -> a
elemAt = undefined

lookupIndex :: Ord a => a -> Ascending a -> Maybe Int
lookupIndex = undefined

