{-# LANGUAGE TemplateHaskell #-}

module Test.SequentialIndex
where

import Data.Maybe
import Data.SequentialIndex
import Test.QuickCheck
import Test.QuickCheck.All
       
instance Arbitrary SequentialIndex where
    arbitrary = fmap fromJust (fmap tryFromBools arbitrary `suchThat` isJust)

prop_sequentialIndexFromToByteString si = Just si == fromByteString (toByteString si) 

prop_between a b = let m = between a b in (a == m && m == b) || (a < m && m < b) || (b < m && m < a)

prop_leftChild x = case leftChild x of
                     Nothing -> True
                     Just v -> v < x

prop_leftChildZero = leftChild zero == Nothing
prop_leftChildOne = leftChild one == Just root
prop_leftChildRoot = isJust $ leftChild root

prop_rightChild x = case rightChild x of
                    Nothing -> True
                    Just v -> v > x

prop_rightChildZero = rightChild zero == Nothing
prop_rightChildOne = rightChild one == Nothing
prop_rightChildRoot = isJust $ rightChild root

main = $(quickCheckAll)

testWith = $(forAllProperties)
