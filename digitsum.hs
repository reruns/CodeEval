import System.Environment
digitsum 0 = 0
digitsum x = x `mod` 10 + digitsum (x `div` 10)

main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map (show.digitsum.read) (lines input)