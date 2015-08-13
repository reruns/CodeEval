import System.Environment

--this is the straightforward approach
ways 0 = 1
ways n = let squares = takeWhile (<=n) sqs
             diffs = map (n-) (reverse squares)
         in (psquares diffs) `div` 2

sqs = zipWith (*) [0..] [0..]

psquares (x:xs) = psq 0 (x:xs) (dropWhile (<x) sqs) where
    psq n [] _ = n
    psq n (x:xs) (y:ys) | x == y = psq (n+1) xs ys
                        | x > y = psq n (x:xs) ys
                        | x < y = psq n xs (y:ys)
                        
--Qiaochu Yuan notes that the prime factorization of N tells
--us its prime factorization over Gaussian Integers
  
main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map (show.ways.read) (tail $ lines input)