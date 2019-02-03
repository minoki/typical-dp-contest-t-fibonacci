{-# LANGUAGE BangPatterns #-}
module PolyMul where
import Data.Int (Int64)
import qualified Data.Vector as V
import Data.List (tails)

modulo = 1000000007 :: Int64
newtype N = N { getModuloN :: Int64 } deriving (Eq)
instance Num N where
  N x + N y = N ((x + y) `mod` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N (x * y `mod` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined
  signum = undefined
instance Show N where
  show (N x) = show x

-- 多項式は
--   V.fromList [a,b,c,...,z] = a + b * X + c * X^2 + ... + z * X^(k-1)
-- により表す。

-- 多項式を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
reduce :: (Eq a, Num a) => Int -> V.Vector a -> V.Vector a
reduce k v | V.last v == 0 = V.init v
           | V.length v <= k = v
           | otherwise = let b = V.last v
                             l = V.length v
                         in reduce k (V.imap (\i a -> if i >= l - k - 1 then a + b else a) (V.init v))

-- 多項式の積を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulP :: (Eq a, Num a) => Int -> V.Vector a -> V.Vector a -> V.Vector a
mulP k !v !w = reduce k $ V.generate (V.length v + V.length w - 1) $
               \i -> sum [(v V.! (i-j)) * (w V.! j) | j <- [0..V.length w-1], j <= i, j > i - V.length v]

-- 不定元
ind :: (Num a) => V.Vector a
ind = V.fromList [0,1]

-- 多項式の（mod X^k - X^(k-1) - ... - X - 1 での）べき乗
powP :: (Eq a, Num a) => Int -> V.Vector a -> Int -> V.Vector a
powP k _ 0 = V.fromList [1]
powP k m i = loop (i-1) m m
  where
    loop 0 !_ !acc = acc
    loop 1 m acc = mulP k m acc
    loop i m acc = case i `quotRem` 2 of
                     (j,0) -> loop j (mulP k m m) acc
                     (j,_) -> loop j (mulP k m m) (mulP k acc m)

main :: IO ()
main = do
  l <- getLine
  let [(k, l')] = (reads :: ReadS Int) l
      [(n, _)] = (reads :: ReadS Int) l'
  if n <= k
    then print 1
    else do
    let f = powP k ind (n - k) -- X^(n-k) mod X^k - X^(k-1) - ... - X - 1
    let seq = replicate k 1 ++ map (sum . take k) (tails seq) -- 数列
    print $ getModuloN $ sum $ zipWith (*) (V.toList f) (drop (k-1) seq)
