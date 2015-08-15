--this isn't fast enough
--memoization is a good choice
import System.Environment

ugly x = any (\y -> x `mod` y == 0) [2,3,5,7]

(<<) :: Int -> Int -> Int
a << b = a*10 + b

data Operation = Shift | Add | Sub deriving Show

apply :: [Int] -> [Operation] -> Int
apply [x] [] = x
apply (x:y:xs) (Shift:os) = apply ((x << y):xs) os
apply (x:xs) (Add:os) = x + (apply xs os)
apply (x:xs) (Sub:os) = x - (apply xs os)

ops 0 = [[]]
ops n = concat [ map (x:) (ops (n-1)) | x <- [Add, Sub, Shift]]

solve x = let digits = map (\x -> read [x] :: Int) x
              os = ops (length x - 1)
          in show $ length $ filter ugly $ map (apply digits) os
          
main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map solve (lines input)