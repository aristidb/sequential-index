module Data.SequentialIndex.Open
where

import           Data.Bits
import           Prelude              hiding (exponent)
import qualified Data.SequentialIndex as Closed

newtype SequentialIndex = OSI Closed.SequentialIndex
    deriving (Eq, Ord)

mantissa :: SequentialIndex -> Integer
mantissa = Closed.mantissa . toClosed

exponent :: SequentialIndex -> Int
exponent = Closed.exponent . toClosed

sequentialIndex ::  Int -> Integer -> SequentialIndex
sequentialIndex eb me = OSI $ Closed.prefixBits eb me Closed.one

toClosed :: SequentialIndex -> Closed.SequentialIndex
toClosed (OSI si) = si

fromClosed :: Closed.SequentialIndex -> Maybe SequentialIndex
fromClosed si = case () of
                  _ | si == Closed.zero -> Nothing
                    | si == Closed.one  -> Nothing
                    | otherwise         -> Just $ OSI si

root :: SequentialIndex
root = OSI Closed.root

leftChild :: SequentialIndex -> SequentialIndex
leftChild = OSI . Closed.leftChild . toClosed

rightChild :: SequentialIndex -> SequentialIndex
rightChild = OSI . Closed.rightChild . toClosed

instance Show SequentialIndex where
    show si = '*' : map (\i -> if testBit m i then 'R' else 'L') [e - 2, e - 3 .. 1]
        where m = mantissa si
              e = exponent si
