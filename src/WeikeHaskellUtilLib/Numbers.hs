module WeikeHaskellUtilLib.Numbers
    (
        divisors,
        isPrime,
        primes,
        isqrt,
        takePrimes
    ) where


divisors :: Int -> [Int]
divisors 0 = [1..]
divisors n | n < 0     = error "n must be greater than 0"
        | otherwise = [ d | d <- [1..n], (n `mod` d) == 0]

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = (length (divisors n)) <= 2

primes :: [Int]
primes = 2:3:[p | p <- [5,7..], not $ primeDividable p]

primeDividable :: Int -> Bool
primeDividable n = or $ map (\x -> mod n x == 0) ps
    where
        ps = takePrimes (div n 2)


takePrimes :: Int -> [Int]
takePrimes n = takePrimes' n primes
    where
        takePrimes' :: Int -> [Int] -> [Int]
        takePrimes' n' (x:xs) = if x < n' then x:(takePrimes' n' xs) else []
        takePrimes' _ _ = error "Something went wrong"

isqrt :: Int -> Int
isqrt = ceiling . sqrt . fromIntegral
