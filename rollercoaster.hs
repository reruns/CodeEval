import Data.Char
import System.Environment
rollercoaster text = rcr text True where
    rcr [] _ = ""
    rcr (x:xs) b | not $ isAlpha x = x: (rcr xs b)
                 | b = (toUpper x):(rcr xs False)
                 | otherwise = (toLower x):(rcr xs True)
                 
main = do [inpFile] <- getArgs
          input <- readFile inpFile
          mapM_ putStrLn $ map rollercoaster (lines input)