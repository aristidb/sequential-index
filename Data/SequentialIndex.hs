module Data.SequentialIndex
(
  SequentialIndex
, zero
, one
)
where

import Data.Bits
import Data.Char
import Data.Word

-- TODO: use Word64 on 64-bit platforms
type Element = Word32

elementSize :: Int
elementSize = bitSize (undefined :: Element)

newtype SequentialIndex = SI [Word32]
    deriving (Eq, Ord)

instance Bounded SequentialIndex where
    minBound = zero
    maxBound = one

instance Show SequentialIndex where
    show (SI xs) = case map showBit $ go xs of 
                     [] -> "0.0"
                     [d1] -> d1 : ".0"
                     (d1:ds) -> d1 : '.' : ds
        where go [] = []
              go [lastElement] = lbits lastElement
              go (el:els) = bits el ++ go els

              lbits el = reverse . dropWhile not $ map (testBit el) [0 .. elementSize - 1]
              bits el = map (testBit el) [elementSize - 1, elementSize - 2 .. 0]

              showBit False = '0'
              showBit True = '1'
              
zero :: SequentialIndex
zero = SI []

one :: SequentialIndex
one = SI [1 `shiftL` (elementSize - 1)]
