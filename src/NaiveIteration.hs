module NaiveIteration where
import Data.Int (Int64)
import Data.List (foldl', tails)

modulo = 1000000007 :: Int64
addMod x y = (x + y) `rem` modulo
sumMod = foldl' addMod 0

main :: IO ()
main = do
  l <- getLine
  let [(k, l')] = (reads :: ReadS Int) l
      [(n, _)] = (reads :: ReadS Int) l'
  let seq = replicate k 1 ++ map (sumMod . take k) (tails seq)
  print $ seq !! (n - 1)

printAsList :: IO ()
printAsList = do
  l <- getLine
  let [(k, l')] = (reads :: ReadS Int) l
      [(n, _)] = (reads :: ReadS Int) l'
  let seq = replicate k 1 ++ map (sumMod . take k) (tails seq)
  print $ take n seq
