{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Ascension
  ( Delimiter(..)
  , Ascension(ascSet, ascDelimiter, ascList)
  , fromDistinctAscList
  , toDistinctAscList
  , forceUntil
  , forceBoth

  , difference
  , intersection
  -- , filter
  -- , partition
  -- , uncons
  -- , mapMonotonic
  -- , member

  , isValid
  ) where

import Prelude hiding (span, filter)
import qualified Data.List as L

import Data.Data
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.Exts

import Data.Ascension.DistinctAscList (DAL(..))
import qualified Data.Ascension.DistinctAscList as DAL

data Delimiter a = Bottom | Middle a | Top
  deriving (Eq, Ord, Show, Data)

data Ascension a = Ascension
  { ascSet       :: Set a
  , ascDelimiter :: Delimiter a
  , ascList      :: DAL a
  }
  deriving (Show, Data)

isValid :: Ord a => Ascension a -> Bool
isValid (Ascension s d l) = case d of
  Bottom   -> S.null s
  Middle x -> maybe True (<= x) (S.lookupMax s)
           && maybe True ((> x) . fst) (DAL.uncons l)
  Top      -> DAL.null l

instance Ord a => IsList (Ascension a) where
  type (Item (Ascension a)) = a

  -- | Strict. See 'fromDistinctAscList' for lazy construction.
  fromList xs = Ascension (S.fromList xs) Top mempty

  toList = toDistinctAscList

-- Lazy. See 'fromList' for strict construction.
fromDistinctAscList :: [a] -> Ascension a
fromDistinctAscList = Ascension S.empty Bottom . DAL

toDistinctAscList :: Ascension a -> [a]
toDistinctAscList (Ascension s _ (DAL l)) = S.toAscList s <> l

instance Ord a => Eq (Ascension a) where
  a1 == a2 = toDistinctAscList a1 == toDistinctAscList a2

instance Ord a => Ord (Ascension a) where
  a1 `compare` a2 = toDistinctAscList a1 `compare` toDistinctAscList a2

forceUntil :: Ord a => a -> Ascension a -> Ascension a
forceUntil x a@(Ascension s d (DAL l))
  | Middle x <= d = a
  | otherwise     = case ns of
    -- pattern match by weak normal form only
    []     -> Ascension s' Top mempty
    n : os -> Ascension (S.insert n s') (Middle n) (DAL os)
    where
      -- by definition of L.span, n is in a weak normal form
      (ms, ns) = L.span (<= x) l
      s' = s <> S.fromDistinctAscList ms

forceBoth :: Ord a => Ascension a -> Ascension a -> (Ascension a, Ascension a)
forceBoth a1@(Ascension s1 d1 _) a2@(Ascension s2 d2 _)
  | x1 <= d2
  , x2 <= d1  = if d1 <= d2
                then (a1, a2 { ascDelimiter = d1 })
                else (a1 { ascDelimiter = d2 }, a2)
  | x1 <= d2  = forceBothHelper x2 a1 a2
  | x2 <= d1  = swap $ forceBothHelper x1 a2 a1
  | otherwise = error "forceBoth: impossible case"
  where
    x1 = maybe Bottom Middle (S.lookupMax s1)
    x2 = maybe Bottom Middle (S.lookupMax s2)

forceBothHelper :: Ord a => Delimiter a -> Ascension a -> Ascension a -> (Ascension a, Ascension a)
forceBothHelper x2 (Ascension s1 _ (DAL l1)) a2@(Ascension _ d2 _) = case ns of
  -- pattern match by weak normal form only
  [] -> (Ascension (s1 <> S.fromDistinctAscList l1) d2 mempty, a2)
  n : os
    | Middle n <= d2
    -> ( Ascension (S.insert n (s1 <> S.fromDistinctAscList ms)) (Middle n) (DAL os)
       , a2 { ascDelimiter = Middle n }
       )
    | otherwise
    -> ( Ascension (s1 <> S.fromDistinctAscList ms) x2 (DAL ns)
       , a2 { ascDelimiter = x2 }
       )
  where
    -- by definition of L.span, n is in a weak normal form
    (ms, ns) = L.span ((<= x2) . Middle) l1

instance Ord a => Semigroup (Ascension a) where
  a1 <> a2 = let (Ascension s1 d l1, Ascension s2 _ l2) = forceBoth a1 a2 in
    Ascension (s1 <> s2) d (l1 <> l2)

instance Ord a => Monoid (Ascension a) where
  mempty  = Ascension mempty Top mempty
  mappend = (<>)

instance Foldable Ascension where
  foldMap f (Ascension s _ l) = foldMap f s <> foldMap f l

difference :: Ord a => Ascension a -> Ascension a -> Ascension a
difference a1 a2 = let (Ascension s1 d l1, Ascension s2 _ l2) = forceBoth a1 a2 in
  Ascension (s1 `S.difference` s2) d (DAL.difference l1 l2)

intersection :: Ord a => Ascension a -> Ascension a -> Ascension a
intersection a1 a2 = let (Ascension s1 d l1, Ascension s2 _ l2) = forceBoth a1 a2 in
  Ascension (s1 `S.intersection` s2) d (DAL.intersection l1 l2)

-- uncons :: Ascension a -> Maybe (a, Ascension a)
-- uncons (Ascension s d l) = case S.minView s of
--   Nothing -> case DAL.uncons l of
--     Nothing      -> Nothing
--     Just (x, xs) -> Just (x, Ascension S.empty d xs)
--   Just (x, s') -> Just (x, Ascension s' d l)

-- mapMonotonic :: (a -> b) -> Ascension a -> Ascension b
-- mapMonotonic f (Ascension s d l) = Ascension (S.mapMonotonic f s) undefined (DAL.mapMonotonic f l)

-- member :: Ord a => a -> Ascension a -> Bool
-- member x (Ascension s _ l) = case S.lookupMax s of
--   Nothing -> DAL.member x l
--   Just y  -> case x `compare` y of
--     LT -> S.member x s
--     EQ -> True
--     GT -> DAL.member x l

-- splitAt :: Int -> Ascension a -> Ascension a
-- splitAt = undefined

-- span :: (a -> Bool) -> Ascension a -> (Ascension a, Ascension a)
-- span = undefined

-- partition :: (a -> Bool) -> Ascension a -> (Ascension a, Ascension a)
-- partition predicate (Ascension s d l) = (Ascension s1 d l1, Ascension s2 d l2)
--   where
--     (s1, s2) = S.partition predicate s
--     (l1, l2) = DAL.partition predicate l

-- filter :: (a -> Bool) -> Ascension a -> Ascension a
-- filter = (fst .) . partition

-- elemAt :: Int -> Ascension a -> a
-- elemAt = undefined

-- lookupIndex :: Ord a => a -> Ascension a -> Maybe Int
-- lookupIndex = undefined

