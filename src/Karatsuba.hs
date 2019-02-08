{-# LANGUAGE BangPatterns #-}
module Karatsuba where
import Data.Int (Int64)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.List (foldl',tails)
import Data.Bits ((.|.))
import Control.Monad (forM_,when)
import Control.Monad.ST (ST)

modulo = 1000000007 :: Int64
addMod !x !y = (x + y) `rem` modulo
subMod !x !y = (x - y) `mod` modulo
negateMod !y = (modulo - y) `rem` modulo
mulMod !x !y = (x * y) `rem` modulo
sumMod xs = (foldl' (+) 0 xs) `rem` modulo
-- sumMod xs = foldl' addMod 0 xs

-- 多項式は
--   V.fromList [a,b,c,...,z] = a + b * X + c * X^2 + ... + z * X^(k-1)
-- により表す。

-- 多項式を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
reduceM :: Int -> VM.MVector s Int64 -> ST s (VM.MVector s Int64)
reduceM !k !v = loop (VM.length v)
  where loop !l | l <= k = return (VM.take l v)
                | otherwise = do b <- VM.read v (l - 1)
                                 forM_ [l - k - 1 .. l - 2] $ \i -> do
                                   VM.modify v (addMod b) i
                                 loop (l - 1)

-- 多項式の和
addP :: V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
addP !v !w | V.length v <= V.length w = V.generate (V.length w) $ \i -> if i < V.length v
                                                                        then (v V.! i) `addMod` (w V.! i)
                                                                        else w V.! i
           | otherwise = V.generate (V.length v) $ \i -> if i < V.length w
                                                         then (v V.! i) `addMod` (w V.! i)
                                                         else v V.! i

-- 多項式の差
subP :: V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
subP !v !w | V.length v <= V.length w = V.generate (V.length w) $ \i -> if i < V.length v
                                                                        then (v V.! i) `subMod` (w V.! i)
                                                                        else negateMod (w V.! i)
           | otherwise = V.generate (V.length v) $ \i -> if i < V.length w
                                                         then (v V.! i) `subMod` (w V.! i)
                                                         else v V.! i

naiveMulP :: V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
naiveMulP !v !w =
  let !vl = V.length v
      !wl = V.length w
  in V.generate (vl + wl - 1) $
     \i -> sumMod [(v V.! (i-j)) `mulMod` (w V.! j) | j <- [max 0 (i - vl + 1) .. min (wl - 1) i]]

at :: V.Vector Int64 -> Int -> Int64
at v i = if i < V.length v then v V.! i else 0

-- Karatsuba multiplication
-- n is a power of 2
-- V.length v <= n, V.length w <= n
doMulP :: Int -> V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
doMulP !n !v !w | n <= 16 = naiveMulP v w
doMulP n !v !w
  | V.null v = v
  | V.null w = w
  | V.length v < n2 = let (w0, w1) = V.splitAt n2 w
                          u0 = doMulP n2 v w0
                          u1 = doMulP n2 v w1
                      in V.generate (V.length v + V.length w - 1)
                         $ \i -> case () of
                                   _ | i < n2     ->  u0 `at` i
                                     | i < n      -> (u0 `at` i) `addMod` (u1 `at` (i - n2))
                                     | i < n + n2 -> (u1 `at` (i - n2))
  | V.length w < n2 = let (v0, v1) = V.splitAt n2 v
                          u0 = doMulP n2 v0 w
                          u1 = doMulP n2 v1 w
                      in V.generate (V.length v + V.length w - 1)
                         $ \i -> case () of
                                   _ | i < n2     ->  u0 `at` i
                                     | i < n      -> (u0 `at` i) `addMod` (u1 `at` (i - n2))
                                     | i < n + n2 -> (u1 `at` (i - n2))
  | otherwise = let (v0, v1) = V.splitAt n2 v
                    (w0, w1) = V.splitAt n2 w
                    v0_1 = v0 `addP` v1
                    w0_1 = w0 `addP` w1
                    p = doMulP n2 v0_1 w0_1
                    q = doMulP n2 v0 w0
                    r = doMulP n2 v1 w1
                    -- s = (p `subP` q) `subP` r -- p - q - r
                    -- q + s*X^n2 + r*X^n
                in V.generate (V.length v + V.length w - 1)
                   $ \i -> case () of
                             _ | i < n2     ->   q `at` i
                               | i < n      -> ((q `at` i) `addMod` (p `at` (i - n2))) `subMod` ((q `at` (i - n2)) `addMod` (r `at` (i - n2)))
                               | i < n + n2 -> ((r `at` (i - n)) `addMod` (p `at` (i - n2))) `subMod` ((q `at` (i - n2)) `addMod` (r `at` (i - n2)))
                               | otherwise  ->   r `at` (i - n)
  where n2 = n `quot` 2

-- 多項式の積を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulP :: Int -> V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
mulP !k !v !w = {- V.force $ -} V.create $ do
  let !vl = V.length v
      !wl = V.length w
      n = ceiling ((log (fromIntegral (vl .|. wl)) :: Double) / log 2) :: Int
  t <- V.thaw (doMulP (2^n) v w)
  reduceM k t

-- 多項式に X をかけたものを X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulByX :: Int -> V.Vector Int64 -> V.Vector Int64
mulByX !k !v
  | V.length v == k = let !v_k = v V.! (k-1)
                      in V.generate k $ \i -> if i == 0
                                              then v_k
                                              else v_k `addMod` (v V.! (i - 1))
  | otherwise = V.cons 0 v

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
