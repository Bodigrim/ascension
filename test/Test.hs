{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.HUnit as H
import Test.Tasty.QuickCheck as QC

import Data.Function (fix)
import Data.List (nub, sort)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Word

import Data.Ascension
import GHC.Exts

instance (Ord a, Arbitrary a) => Arbitrary (Ascension a) where
  arbitrary = do
    xs <- arbitrary
    x  <- arbitrary
    pure $ forceUntil x (fromDistinctAscList (nub (sort xs)))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ QC.testProperty "isValid" $
    \(xs :: Ascension Word8) -> isValid xs
  , QC.testProperty "fromList" $
    \(xs :: [Word8]) -> isValid (fromList xs)
  , QC.testProperty "fromDistinctAscList" $
    \(xs :: [Word8]) -> isValid (fromDistinctAscList (nub (sort xs)))

  , QC.testProperty "toList1" $
    \(xs :: [Word8]) -> toList (fromList xs :: Ascension Word8) == nub (sort xs)
  , QC.testProperty "toList2" $
    \(xs :: [Word8]) -> let xs' = nub (sort xs)
      in toList (fromDistinctAscList xs' :: Ascension Word8) == xs'
  , QC.testProperty "toList3" $
    \(xs :: Ascension Word8) -> fromList (toList xs) == xs
  , QC.testProperty "toList4" $
    \(xs :: Ascension Word8) -> fromDistinctAscList (toList xs) == xs

  , QC.testProperty "forceUntil" $
    \(xs :: Ascension Word8)  x -> let ys = forceUntil x xs
      in isValid ys
      .&. ys === xs
      .&. ascDelimiter ys >= Middle x

  , QC.testProperty "forceBoth" $
    \(xs :: Ascension Word8) ys -> let (xs', ys') = forceBoth xs ys
      in  isValid xs'
      .&. isValid ys'
      .&. xs' === xs
      .&. ys' === ys
      .&. ascDelimiter xs' === ascDelimiter ys'

  , QC.testProperty "mappend" $
    \(xs :: Ascension Word8) ys -> let zs = xs <> ys
      in isValid zs
      .&. toList zs === nub (sort (toList xs <> toList ys))

  , QC.testProperty "difference" $
    \(xs :: Ascension Word8) ys -> let zs = xs `difference` ys
      in isValid zs
      .&. toList zs === toList (S.fromDistinctAscList (toList xs) `S.difference` S.fromDistinctAscList (toList ys))

  , QC.testProperty "intersection" $
    \(xs :: Ascension Word8) ys -> let zs = xs `intersection` ys
      in isValid zs
      .&. toList zs === toList (S.fromDistinctAscList (toList xs) `S.intersection` S.fromDistinctAscList (toList ys))

  , QC.testProperty "foldMap" $
    \(xs :: Ascension Word8) -> foldMap (: []) xs === toList xs
  ]
