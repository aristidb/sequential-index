module Data.SequentialIndex
(
-- * Type
  SequentialIndex
-- * Extractors
, mantissa
, exponent
-- * Constructors
, zero
, one
, root
, sequentialIndex
, unsafeSequentialIndex
, tryFromBools
-- * Operations
-- ** Arithmetical operations
, between
, prefixBits
-- ** Tree operations
, build
, buildBits
, leftChild
, rightChild
, parent
-- * Conversion
, toByteString
, fromByteString
)
where

import           Data.Bits
import           Prelude         hiding (exponent)
import qualified Data.ByteString as B

-- | An arbitrary-precision number between 0.0 and 1.0. To create new numbers,
-- use 'between'.
-- 
-- Each number consist of a 'mantissa' (>= 0) and an 'exponent' (> 0), so that
-- its numeric value equals @mantissa x * 2 ^ (1 - exponent x)@. The constraint
-- that it must lie between 0.0 and 1.0 is enforced in the constructors.
-- 
-- It is possible to span a hypothetical tree in this number scheme. Discarding
-- the last binary digit of the mantissa, which has to be a 1, each digit of
-- the mantissa denotes a branch in this hypothetical binary tree. So a whole
-- 'SequentialIndex' (if it ends with 1) corresponds with a path in a binary
-- tree.
data SequentialIndex 
    = SI !Integer !Int -- must always be in normalised form!
    deriving (Eq)

-- | Extracts the mantissa.
mantissa :: SequentialIndex -> Integer
mantissa (SI m _) = m

-- | Extracts the exponent.
exponent :: SequentialIndex -> Int
exponent (SI _ e) = e

-- | The lowest possible number: 0.0.
zero :: SequentialIndex
zero = SI 0 1

-- | The highest possible number: 1.0.
one :: SequentialIndex
one = SI 1 1

-- | The root of a hypothetical binary tree.
root :: SequentialIndex
root = between zero one

commonBase :: SequentialIndex -> SequentialIndex -> (Integer, Integer, Int)
commonBase (SI m1 e1) (SI m2 e2) = (m1', m2', e)
    where e = max e1 e2
          m1' = m1 `shift` (e - e1)
          m2' = m2 `shift` (e - e2)

-- | Construct a 'SequentialIndex' from its 'mantissa' and 'exponent'.
-- 
-- Errors are checked and result in a run-time error.
sequentialIndex :: Integer -> Int -> SequentialIndex
sequentialIndex 0 _ = zero
sequentialIndex mx ex
    = case unsafeSequentialIndex mx ex of
        v | v < zero  -> error "Invalid SequentialIndex: below zero"
          | v > one   -> error "Invalid SequentialIndex: beyond one"
          | otherwise -> v

-- | Construct a 'SequentialIndex' from its 'mantissa' and 'exponent'.
-- 
-- Errors are checked and result in 'Nothing'.
trySequentialIndex :: Integer -> Int -> Maybe SequentialIndex
trySequentialIndex 0 _ = Just zero
trySequentialIndex mx ex
    = case unsafeSequentialIndex mx ex of
        v | v < zero  -> Nothing
          | v > one   -> Nothing
          | otherwise -> Just v

-- | Construct a 'SequentialIndex' from its 'mantissa' and 'exponent'.
-- 
-- Errors are not checked.
unsafeSequentialIndex :: Integer -> Int -> SequentialIndex
unsafeSequentialIndex mx ex 
    = until (\(SI m _) -> m `testBit` 0) 
            (\(SI m e) -> SI (m `shiftR` 1) (e - 1)) 
            (SI mx ex)

-- | Construct a 'SequentialIndex' from a list of boolean digits. The exponent
-- equals the number of digits.
tryFromBools :: [Bool] -> Maybe SequentialIndex
tryFromBools = uncurry trySequentialIndex . foldr (\x (s, n) -> ((s `shiftL` 1) .|. (if x then 1 else 0), n + 1)) (0, 0)

-- | Compute a number right in the middle of the arguments.
-- 
-- @(x + y) / 2@
between :: SequentialIndex -> SequentialIndex -> SequentialIndex
between a b = sequentialIndex (m1 + m2) (e + 1)
    where (m1, m2, e) = commonBase a b

-- | Add digits in front of the mantissa.
prefixBits :: Int -> Integer -> SequentialIndex -> SequentialIndex
prefixBits _ _   (SI 0 1) = error "No meaningful prefix for 'zero' possible"
prefixBits eb mb (SI m e) = sequentialIndex ((mb `shiftL` e) + m) (eb + e + 1)

-- | Build a number from a list of fixed-width mantissa segments.
build :: Int -> [Integer] -> SequentialIndex
build nbits xs = foldr (prefixBits nbits) one xs

-- | Build a number from a list of fixed-width mantissa segments.
buildBits :: (Bits a, Integral a) => [a] -> SequentialIndex
buildBits xs = build (bitSize $ head xs) (map toInteger xs)

-- | Get the left child of the current path in the hypothetical tree.
leftChild :: SequentialIndex -> Maybe SequentialIndex
leftChild (SI 0 1) = Nothing
leftChild (SI m e) = Just $ SI ((m `shiftR` 1) `shiftL` 2 .|. 1) (e + 1)

-- | Get the right child of the current path in the hypothetical tree.
rightChild :: SequentialIndex -> Maybe SequentialIndex
rightChild (SI _ 1) = Nothing
rightChild (SI m e) = Just $ SI ((m `shiftR` 1) `shiftL` 2 .|. 3) (e + 1)

-- | Get the parent of the current path in the hypothetical tree.
parent :: SequentialIndex -> Maybe SequentialIndex
parent (SI _ 1) = Nothing
parent (SI m e) = Just $ SI ((m `shiftR` 2) `shiftL` 1 .|. 1) (e - 1)

-- | Convert a 'SequentialIndex' to a binary representation.
toByteString :: SequentialIndex -> B.ByteString
toByteString (SI m e) = B.unfoldr step (m', e')
    where e' = ((e + 7) `div` 8) * 8
          m' = m `shift` (e' - e)

          step (_, 0) = Nothing
          step (v, ex) = let (q, r) = v `divMod` 256
                         in Just (fromInteger r, (q, ex - 8))

-- | Convert a 'SequentialIndex' from its binary representation.
fromByteString :: B.ByteString -> Maybe SequentialIndex
fromByteString bs | B.null bs = Just zero
                  | otherwise = trySequentialIndex m e
    where (m, e) = B.foldr step (0, 0) bs
          step w (mx, ex) = (mx `shiftL` 8 + toInteger w, ex + 8)


instance Bounded SequentialIndex where
    minBound = zero
    maxBound = one

instance Ord SequentialIndex where
    a `compare` b = a' `compare` b'
        where (a', b', _) = commonBase a b

instance Show SequentialIndex where
    show (SI m e) = case map sbit bits of
                      []      -> "0.0"
                      [d1]    -> d1 : ".0"
                      (d1:ds) -> d1 : '.' : ds
        where bits = map (testBit m) [e - 1, e - 2 .. 0]
              
              sbit False = '0'
              sbit True  = '1'
