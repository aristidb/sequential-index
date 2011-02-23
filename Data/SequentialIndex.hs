module Data.SequentialIndex
(
  SequentialIndex
, mantissa
, exponent
, zero
, one
, sequentialIndex
, between
, toByteString
, fromByteString
)
where

import           Data.Bits
import           Prelude         hiding (exponent)
import qualified Data.ByteString as B

-- must always be in normalised form!
data SequentialIndex 
    = SI !Integer !Int
    deriving (Eq)

mantissa :: SequentialIndex -> Integer
mantissa (SI m _) = m

exponent :: SequentialIndex -> Int
exponent (SI _ e) = e

zero :: SequentialIndex
zero = SI 0 0

one :: SequentialIndex
one = SI 1 0

commonBase :: SequentialIndex -> SequentialIndex -> (Integer, Integer, Int)
commonBase (SI m1 e1) (SI m2 e2) = (m1', m2', e)
    where e = max e1 e2
          m1' = m1 `shift` (e - e1)
          m2' = m2 `shift` (e - e2)

sequentialIndex :: Integer -> Int -> SequentialIndex
sequentialIndex 0 _ = zero
sequentialIndex mx ex
    = case () of
        _ | v < zero  -> error "Invalid SequentialIndex: below zero"
          | v > one   -> error "Invalid SequentialIndex: beyond one"
          | otherwise -> v
    where v = until (\(SI m _) -> m `testBit` 0) 
                    (\(SI m e) -> SI (m `shiftR` 1) (e - 1)) 
                    (SI mx ex)

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
        where bits = map (testBit m) [e, e - 1 .. 0]
              
              sbit False = '0'
              sbit True  = '1'

between :: SequentialIndex -> SequentialIndex -> SequentialIndex
between a b = sequentialIndex (m1 + m2) (e + 1)
    where (m1, m2, e) = commonBase a b

toByteString :: SequentialIndex -> B.ByteString
toByteString (SI m e) = B.unfoldr step m'
    where e' = (e `div` 8) * 8 + 7
          m' = m `shift` (e' - e)

          step 0 = Nothing
          step v = let (q, r) = v `divMod` 256
                   in Just (fromInteger r, q)

fromByteString :: B.ByteString -> SequentialIndex
fromByteString bs = sequentialIndex m (max 0 $ e - 1)
    where (m, e) = B.foldr step (0, 0) bs
          step w (mx, ex) = (mx `shiftL` 8 + toInteger w, ex + 8)
