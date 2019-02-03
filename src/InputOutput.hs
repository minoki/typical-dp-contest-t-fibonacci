module InputOutput where

main :: IO ()
main = do
  l <- getLine
  let [(k, l')] = (reads :: ReadS Int) l
      [(n, _)] = (reads :: ReadS Int) l'
  putStrLn $ "K = " ++ show k ++ ", N = " ++ show n
