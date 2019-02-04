{-# LANGUAGE BangPatterns #-}
module PolyMulUnboxed where
import Data.Int (Int64)
import qualified Data.Vector.Unboxed as V
import Data.List (foldl',tails)

modulo = 1000000007 :: Int64
addMod x y = (x + y) `rem` modulo
mulMod x y = (x * y) `rem` modulo
sumMod = foldl' addMod 0

-- 多項式は
--   V.fromList [a,b,c,...,z] = a + b * X + c * X^2 + ... + z * X^(k-1)
-- により表す。

-- 多項式を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
reduce :: Int -> V.Vector Int64 -> V.Vector Int64
reduce k v | V.last v == 0 = V.init v
           | V.length v <= k = v
           | otherwise = let b = V.last v
                             l = V.length v
                         in reduce k (V.imap (\i a -> if i >= l - k - 1 then a `addMod` b else a) (V.init v))

-- 多項式の積を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulP :: Int -> V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
mulP k !v !w = reduce k $ V.generate (V.length v + V.length w - 1) $
               \i -> sumMod [(v V.! (i-j)) `mulMod` (w V.! j) | j <- [0..V.length w-1], j <= i, j > i - V.length v]

-- 不定元
ind :: V.Vector Int64
ind = V.fromList [0,1]

-- 多項式の（mod X^k - X^(k-1) - ... - X - 1 での）べき乗
powP :: Int -> V.Vector Int64 -> Int -> V.Vector Int64
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
    let seq = replicate k 1 ++ map (sumMod . take k) (tails seq) -- 数列
    print $ sumMod $ zipWith mulMod (V.toList f) (drop (k-1) seq)
