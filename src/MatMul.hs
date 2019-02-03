{-# LANGUAGE BangPatterns #-}
module MatMul where
import Data.Array
import Data.Ix (range)
import Data.Int (Int64)

---

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

---

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

matPow :: (Num a) => Int -> Array (Int,Int) a -> Int -> Array (Int,Int) a
matPow k m 0 = array ((1,1),(k,k)) $
               [((i,j), if i == j then 1 else 0) | i <- [1..k], j <- [1..k]]
matPow _ m i = loop (i-1) m m
  where
    loop 0 !_ acc = acc
    loop 1 m acc = m `matMul` acc
    loop i m acc = case i `quotRem` 2 of
                     (j,0) -> loop j (m `matMul` m) acc
                     (j,_) -> loop j (m `matMul` m) (acc `matMul` m)

main :: IO ()
main = do
  l <- getLine
  let [(k, l')] = (reads :: ReadS Int) l
      [(n, _)] = (reads :: ReadS Int) l'
  if n <= k
    then print 1
    else do
    let t = array ((1,1),(k,k)) $
            [((i,j), if i + 1 == j then 1 else 0) | i <- [1..k-1], j <- [1..k]]
            ++ [((k,j),1) | j <- [1..k]]
    let u = matPow k t (n - k) :: Array (Int,Int) N
    print $ getModuloN $ sum [u!(k,j) | j <- [1..k]]
