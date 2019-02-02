{-# LANGUAGE BangPatterns #-}
module MainW where
import Data.Array.Unboxed
import Data.Ix
import Data.Int
import qualified Data.Vector.Unboxed as V
import Data.List

---

newtype N = N { getModuloN :: Int64 } deriving (Eq)
modulo = 1000000007 :: Int64
instance Num N where
  N x + N y = N ((x + y) `mod` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N (x * y `mod` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined
  signum = undefined

instance Show N where
  show (N x) = show x

addMod x y = (x + y) `mod` modulo
mulMod x y = (x * y) `mod` modulo
sumMod = foldl' addMod 0

---

{-
matMul :: (Num a) => Array (Int,Int) a -> Array (Int,Int) a -> Array (Int,Int) a
matMul a b = let ((i0,j0),(ix,jx)) = bounds a
                 ((j'0,k0),(j'x,kx)) = bounds b
             in if jx - j0 == j'x - j'0
                   then array ((i0,k0),(ix,kx))
                        [ ((i,k), sum [a!(i,j) * b!(j',k) | (j,j') <- zip (range (j0,jx)) (range (j'0,j'x))])
                        | i <- range (i0,ix)
                        , k <- range (k0,kx)
                        ]
                else error "Matrix size mismatch"
-}

{-
matMul :: UArray (Int,Int) Int64 -> UArray (Int,Int) Int64 -> UArray (Int,Int) Int64
matMul a b = let ((i0,j0),(ix,jx)) = bounds a
                 ((j'0,k0),(j'x,kx)) = bounds b
             in if jx - j0 == j'x - j'0
                   then array ((i0,k0),(ix,kx))
                        [ ((i,k), getModuloN $ sum [N (a!(i,j)) * N (b!(j',k)) | (j,j') <- zip (range (j0,jx)) (range (j'0,j'x))])
                        | i <- range (i0,ix)
                        , k <- range (k0,kx)
                        ]
                else error "Matrix size mismatch"

--matPow :: (Num a) => Int -> Array (Int,Int) a -> Int -> Array (Int,Int) a
matPow :: Int -> UArray (Int,Int) Int64 -> Int -> UArray (Int,Int) Int64
matPow k m 0 = array ((1,1),(k,k)) $
               [((i,j), if i == j then 1 else 0) | i <- [1..k], j <- [1..k]]
matPow _ m i = loop (i-1) m m
  where
    loop 0 _ acc = acc
    loop i m acc = case i `quotRem` 2 of
                     (j,0) -> loop j (m `matMul` m) acc
                     (j,_) -> loop j (m `matMul` m) (acc `matMul` m)
-}

---

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
  {-let t = array ((1,1),(k,k)) $
          [((i,j), if i + 1 == j then 1 else 0) | i <- [1..k-1], j <- [1..k]]
          ++ [((k,j),1) | j <- [1..k]] -}
  if n <= k
    then print 1
    else do
    let p = powP k ind (n - k)
    let seq = replicate k 1 ++ map (sumMod . take k) (tails seq)
    -- print (take 10 seq)
    -- print (take 10 $ V.toList p)
    -- print $ getModuloN $ sum $ zipWith (*) (V.toList p) (drop (k-1) seq)
    print $ sumMod $ zipWith mulMod (V.toList p) (drop (k-1) seq)
    -- let u = matPow k t (n - k)
    -- print $ getModuloN $ sum [N (u!(k,j)) | j <- [1..k]]
