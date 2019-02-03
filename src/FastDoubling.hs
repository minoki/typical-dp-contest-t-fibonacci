{-# LANGUAGE BangPatterns #-}
module FastDoubling where
import Data.Int (Int64)
import qualified Data.Vector.Unboxed as V
import Data.List (foldl',tails)

modulo = 1000000007 :: Int64
addMod !x !y = (x + y) `mod` modulo
mulMod !x !y = (x * y) `mod` modulo
sumMod = foldl' addMod 0

-- 多項式は
--   V.fromList [a,b,c,...,z] = a + b * X + c * X^2 + ... + z * X^(k-1)
-- により表す。

-- 多項式を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
reduce :: Int -> V.Vector Int64 -> V.Vector Int64
reduce !k !v | V.last v == 0 = V.init v
             | V.length v <= k = v
             | otherwise = let b = V.last v
                               l = V.length v
                           in reduce k (V.imap (\i a -> if i >= l - k - 1 then a `addMod` b else a) (V.init v))

-- 多項式の積を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulP :: Int -> V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
mulP !k !v !w = reduce k $ V.generate (V.length v + V.length w - 1) $
                \i -> sumMod [(v V.! (i-j)) `mulMod` (w V.! j) | j <- [0..V.length w-1], j <= i, j > i - V.length v]

-- 多項式に X をかけたものを X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulByX :: Int -> V.Vector Int64 -> V.Vector Int64
mulByX !k !v
  | V.length v == k = let !v_k = v V.! (k-1)
                      in V.generate k $ \i -> if i == 0
                                              then v_k
                                              else v_k `addMod` (v V.! (i - 1))
  | otherwise = V.generate (V.length v + 1) $ \i -> if i == 0
                                                    then 0
                                                    else v V.! (i - 1)

-- X の（mod X^k - X^(k-1) - ... - X - 1 での）n 乗
powX :: Int -> Int -> V.Vector Int64
powX !k !n = doPowX n
  where
    doPowX 0 = V.fromList [1]   -- 1
    doPowX 1 = V.fromList [0,1] -- X
    doPowX i = case i `quotRem` 2 of
                 (j,0) -> let !f = doPowX j -- X^j mod P
                          in mulP k f f
                 (j,_) -> let !f = doPowX j -- X^j mod P
                          in mulByX k (mulP k f f)

main :: IO ()
main = do
  l <- getLine
  let [(k, l')] = (reads :: ReadS Int) l
      [(n, _)] = (reads :: ReadS Int) l'
  if n <= k
    then print 1
    else do
    let f = powX k (n - k) -- X^(n-k) mod X^k - X^(k-1) - ... - X - 1
    let seq = replicate k 1 ++ map (sumMod . take k) (tails seq) -- 数列
    print $ sumMod $ zipWith mulMod (V.toList f) (drop (k-1) seq)
