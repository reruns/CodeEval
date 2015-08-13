--print the largest prime palindrome less than 1000
primes = 2 : 3 : [n | n<-[5,7..], foldr (\p r-> p*p>n || (rem n p /= 0 && r)) True (tail primes)]
pal = last $ filter isPalindrome $ takeWhile (<1000) primes
isPalindrome x = (show x) == (reverse $ show x)

main = putStrLn $ show pal