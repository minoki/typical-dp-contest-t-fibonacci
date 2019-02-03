{-# LANGUAGE BangPatterns #-}
module MatMulUnboxed where
import Data.Array.Unboxed
import Data.Ix (range)
import Data.Int (Int64)
import Data.List (foldl')

---

modulo = 1000000007 :: Int64
addMod x y = (x + y) `mod` modulo
mulMod x y = (x * y) `mod` modulo
sumMod = foldl' addMod 0

---

matMul :: UArray (Int,Int) Int64 -> UArray (Int,Int) Int64 -> UArray (Int,Int) Int64
matMul a b = let ((i0,j0),(ix,jx)) = bounds a
                 ((j'0,k0),(j'x,kx)) = bounds b
             in if jx - j0 == j'x - j'0
                   then array ((i0,k0),(ix,kx))
                        [ ((i,k), sumMod [(a!(i,j)) `mulMod` (b!(j',k)) | (j,j') <- zip (range (j0,jx)) (range (j'0,j'x))])
                        | i <- range (i0,ix)
                        , k <- range (k0,kx)
                        ]
                else error "Matrix size mismatch"

matPow :: Int -> UArray (Int,Int) Int64 -> Int -> UArray (Int,Int) Int64
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
    let u = matPow k t (n - k) :: UArray (Int,Int) Int64
    print $ sumMod [u!(k,j) | j <- [1..k]]
