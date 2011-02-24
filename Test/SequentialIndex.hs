module Test.SequentialIndex
where

import Data.Maybe
import Data.SequentialIndex
import Test.QuickCheck
       
instance Arbitrary SequentialIndex where
    arbitrary = fmap fromJust (fmap tryFromBools arbitrary `suchThat` isJust)

prop_sequentialIndexFromToByteString si = Just si == fromByteString (toByteString si) 
