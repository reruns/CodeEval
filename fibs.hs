import System.Environment
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibonacci = show.(fibs !!).read

main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map fibonacci (lines input)