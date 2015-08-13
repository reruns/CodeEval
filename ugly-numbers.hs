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

--I think the reason test cases are failing is this:
--ops has to be mostly evaluated before we can use elements of the result
--which takes up a huge amount of memory for longer strings
ops 0 = [[]]
ops x = let os = ops (x-1) in (map (Add:) os) ++ (map (Sub:) os) ++ (map (Shift:) os)

solve x = let digits = map (\x -> read [x] :: Int) x
              os = ops (length x - 1)
          in show $ length $ filter ugly $ map (apply digits) os
          
main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map solve (lines input)