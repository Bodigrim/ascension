{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Ascension.DistinctAscList
  ( DAL(..)
  , intersection
  , difference
  , member
  , null
  , uncons
  , mapMonotonic
  , partition
  ) where

import Prelude hiding (null)
import Data.Coerce (coerce)
import Data.Data (Data)
import qualified Data.List as L

newtype DAL a = DAL [a]
  deriving (Eq, Ord, Show, Foldable, Data)

instance Ord a => Semigroup (DAL a) where
  (<>) = merge

instance Ord a => Monoid (DAL a)
  where
    mempty = DAL []
    mappend = (<>)

merge :: forall a. Ord a => DAL a -> DAL a -> DAL a
merge = coerce go
  where
    go :: [a] -> [a] -> [a]
    go xs [] = xs
    go [] ys = ys
    go xs@(x : xs') ys@(y : ys') = case x `compare` y of
      LT -> x : go xs' ys
      EQ -> x : go xs' ys'
      GT -> y : go xs  ys'

intersection :: forall a. Ord a => DAL a -> DAL a -> DAL a
intersection = coerce go
  where
    go :: [a] -> [a] -> [a]
    go _ [] = []
    go [] _ = []
    go xs@(x : xs') ys@(y : ys') = case x `compare` y of
      LT -> go xs' ys
      EQ -> x : go xs' ys'
      GT -> go xs ys'

difference :: forall a. Ord a => DAL a -> DAL a -> DAL a
difference = coerce go
  where
    go :: [a] -> [a] -> [a]
    go xs [] = xs
    go [] _  = []
    go xs@(x : xs') ys@(y : ys') = case x `compare` y of
      LT -> x : go xs' ys
      EQ -> go xs' ys'
      GT -> go xs ys'

member :: forall a. Ord a => a -> DAL a -> Bool
member x (DAL xs) = go xs
  where
    go :: [a] -> Bool
    go = \case
      [] -> False
      y : ys -> case x `compare` y of
        LT -> False
        EQ -> True
        GT -> go ys

null :: forall a. DAL a -> Bool
null = coerce (L.null :: [a] -> Bool)

uncons :: forall a. DAL a -> Maybe (a, DAL a)
uncons = coerce (L.uncons :: [a] -> Maybe (a, [a]))

mapMonotonic :: (a -> b) -> DAL a -> DAL b
mapMonotonic f = coerce (map f)

partition :: (a -> Bool) -> DAL a -> (DAL a, DAL a)
partition f = coerce (L.partition f)
