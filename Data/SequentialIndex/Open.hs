module Data.SequentialIndex.Open
(
  SequentialIndex
, mantissa
, exponent
, sequentialIndex
, tryFromBools
, toClosed
, fromClosed
, root
, leftChild
, rightChild
, parent
, prefixBits
, toByteString
, fromByteString
)
where

import           Control.Monad
import           Data.Bits
import           Data.Maybe
import           Prelude              hiding (exponent)
import qualified Data.ByteString      as B
import qualified Data.SequentialIndex as Closed

newtype SequentialIndex = OSI Closed.SequentialIndex
    deriving (Eq, Ord)

mantissa :: SequentialIndex -> Integer
mantissa = Closed.mantissa . toClosed

exponent :: SequentialIndex -> Int
exponent = Closed.exponent . toClosed

sequentialIndex ::  Int -> Integer -> SequentialIndex
sequentialIndex eb me = OSI $ Closed.prefixBits eb me Closed.one

tryFromBools :: [Bool] -> Maybe SequentialIndex
tryFromBools = fromClosed <=< Closed.tryFromBools

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
leftChild = OSI . fromJust . Closed.leftChild . toClosed

rightChild :: SequentialIndex -> SequentialIndex
rightChild = OSI . fromJust . Closed.rightChild . toClosed

parent :: SequentialIndex -> Maybe SequentialIndex
parent = fmap OSI . Closed.parent . toClosed

prefixBits :: Int -> Integer -> SequentialIndex -> SequentialIndex
prefixBits eb mb = OSI . Closed.prefixBits eb mb . toClosed

toByteString :: SequentialIndex -> B.ByteString
toByteString = Closed.toByteString . toClosed

fromByteString :: B.ByteString -> Maybe SequentialIndex
fromByteString = fromClosed <=< Closed.fromByteString

instance Show SequentialIndex where
    show si = '*' : map (\i -> if testBit m i then 'R' else 'L') [e - 2, e - 3 .. 1]
        where m = mantissa si
              e = exponent si
