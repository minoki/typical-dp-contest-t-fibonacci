{-# LANGUAGE BangPatterns #-}
module Fast where
-- import Data.Array.Unboxed
-- import Data.Ix
import Data.Int
import qualified Data.Vector.Unboxed as V
import Data.List

modulo = 1000000007 :: Int64
addMod x y = (x + y) `mod` modulo
mulMod x y = (x * y) `mod` modulo
sumMod = foldl' addMod 0

-- V.fromList [a,b,c,...,z] = a + b * X + c * X^2 + ... + z * X^(k-1)

-- reduce :: (Eq a, Num a) => Int -> V.Vector a -> V.Vector a
reduce :: Int -> V.Vector Int64 -> V.Vector Int64
reduce k v | V.last v == 0 = V.init v
           | V.length v <= k = v
           | otherwise = let b = V.last v
                             l = V.length v
                         in reduce k (V.imap (\i a -> if i >= l - k - 1 then a `addMod` b else a) (V.init v))

-- mulP :: (Eq a, Num a) => Int -> V.Vector a -> V.Vector a -> V.Vector a
mulP :: Int -> V.Vector Int64 -> V.Vector Int64 -> V.Vector Int64
mulP k !v !w = reduce k $ V.generate (V.length v + V.length w - 1) $
             \i -> sumMod [(v V.! (i-j)) `mulMod` (w V.! j) | j <- [0..V.length w-1], j <= i, j > i - V.length v]

-- ind :: (Num a) => V.Vector a
ind :: V.Vector Int64
ind = V.fromList [0,1]

-- powP :: (Eq a, Num a) => Int -> V.Vector a -> Int -> V.Vector a
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
    let p = powP k ind (n - k)
    let seq = replicate k 1 ++ map (sumMod . take k) (tails seq)
    print $ take 10 seq
    print $ take 10 (V.toList p)
    print $ sumMod $ zipWith mulMod (V.toList p) (drop (k-1) seq)
