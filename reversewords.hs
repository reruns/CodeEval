import System.Environment
rwords = unwords.reverse.words
main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map rwords (lines input)