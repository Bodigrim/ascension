{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Ascension.DistinctAscList
  ( DAL
  , fromDistinctAscList
  , fromDistinctDescList
  , toSet
  , toIntSet
  , toSetDown
  , intersection
  , difference
  , mergeWith
  , member
  , null
  , uncons
  , mapMonotonic
  , partition
  , span
  , reverse
  , downDown
  ) where

import Prelude hiding (null, reverse, span)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Ord (Down(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Type.Equality ((:~:)(..))
import Unsafe.Coerce (unsafeCoerce)

newtype DAL a = DAL [a]
  deriving (Eq, Ord, Show, Foldable, Data)

type role DAL nominal

fromDistinctAscList :: [a] -> DAL a
fromDistinctAscList = coerce

fromDistinctDescList :: [a] -> DAL (Down a)
fromDistinctDescList = coerce

toSet :: DAL a -> Set a
toSet = coerce S.fromDistinctAscList

toSetDown :: DAL (Down a) -> Set a
toSetDown = coerce S.fromDistinctDescList

toIntSet :: DAL Int -> IntSet
toIntSet = coerce IS.fromDistinctAscList

reverse :: forall a. DAL a -> DAL (Down a)
reverse = coerce (L.reverse :: [a] -> [a])

-- | Use in
-- castWith (Refl `apply` downDown)
downDown :: Down (Down a) :~: a
downDown = unsafeCoerce (Refl :: a :~: a)

instance Ord a => Semigroup (DAL a) where
  (<>) = union

instance Ord a => Monoid (DAL a) where
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

span :: (a -> Bool) -> DAL a -> (DAL a, DAL a)
span f = coerce (L.span f)
