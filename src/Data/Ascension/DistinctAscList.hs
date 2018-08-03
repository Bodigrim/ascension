{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RoleAnnotations     #-}
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

type role DAL nominal

instance Ord a => Semigroup (DAL a) where
  (<>) = union

instance Ord a => Monoid (DAL a)
  where
    mempty = DAL []
    mappend = (<>)

union :: forall a. Ord a => DAL a -> DAL a -> DAL a
union = mergeWith compare id (\x _ -> Just x) id

intersection :: forall a. Ord a => DAL a -> DAL a -> DAL a
intersection = mergeWith compare (\_ -> []) (\x _ -> Just x) (\_ -> [])

difference :: forall a. Ord a => DAL a -> DAL a -> DAL a
difference = mergeWith compare id (\_ _ -> Nothing) (\_ -> [])

mergeWith
  :: forall a b c.
     (a -> b -> Ordering)
  -> ([a] -> [c])
  -> (a -> b -> Maybe c)
  -> ([b] -> [c])
  -> DAL a
  -> DAL b
  -> DAL c
mergeWith cmpr lt eq gt = coerce go
  where
    go :: [a] -> [b] -> [c]
    go xs [] = lt xs
    go [] ys = gt ys
    go xs@(x : xs') ys@(y : ys') = case x `cmpr` y of
      LT -> lt [x] ++ go xs' ys
      EQ -> maybe id (:) (eq x y) (go xs' ys')
      GT -> gt [y] ++ go xs ys'
{-# INLINE mergeWith #-}

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
