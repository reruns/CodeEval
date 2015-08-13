import System.Environment

fizzbuzz st = let [x,y,n] = map read $ words st in fb x y n
fb x y n = unwords $ map (rep x y) [1..n]

rep :: Int -> Int -> Int -> String
rep x y z | (z %% x) && (z %% y) = "FB"
          | z %% x = "F"
          | z %% y = "B"
          | otherwise = show z
          
x %% y = x `mod` y == 0

main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map fizzbuzz (lines input)