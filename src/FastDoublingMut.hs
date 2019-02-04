{-# LANGUAGE BangPatterns #-}
module FastDoublingMut where
import Data.Int (Int64)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.List (foldl',tails)
import Control.Monad (forM_,when)
import Control.Monad.ST (ST)

modulo = 1000000007 :: Int64
addMod !x !y = (x + y) `mod` modulo
mulMod !x !y = (x * y) `mod` modulo
sumMod xs = foldl' addMod 0 xs

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

-- 多項式の積を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulP :: Int -> V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
mulP !k !v !w = {- V.force $ -} V.create $ do
  let !vl = V.length v
      !wl = V.length w
  s <- VM.new (vl + wl - 1)
  forM_ [0 .. vl + wl - 2] $ \i -> do
    let !x = sumMod [(v V.! (i-j)) `mulMod` (w V.! j) | j <- [max 0 (i - vl + 1) .. min (wl - 1) i]]
    VM.write s i x
  reduceM k s

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
