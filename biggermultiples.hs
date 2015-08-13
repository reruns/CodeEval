import System.Environment

biggerMultiples st = let [x,y] = map read $ wordsWhen (==',') st in show $ head $ dropWhile (<x) [y, y*2..]

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
                            
main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map biggerMultiples (lines input)