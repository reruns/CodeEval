import System.Environment

letterify :: Int -> String
letterify x | x <= 26 = [letters !! x]
            | otherwise = letterify ((x-1) `div` 26) ++ [letters !! (x `mod` 26)]

letters = "ZABCDEFGHIJKLMNOPQRSTUVWXYZ"

main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map (letterify.read) (lines input)