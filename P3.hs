--
-- CS 430 Spring 2019 P3 (Haskell 1)
--
-- Name: Nathan Chan
--

module P3 where

-- A list of all factors of n.Prelude. If you are unsure about a particular class or method, you can consult the documentation.
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- True iff n is prime.
isPrime :: Integral a => a -> Bool
isPrime n = length (factors n) == 2
-- (factors n) == [1, n]
    

-- A list of all prime factors of n.
primeFactors :: Integral a => a -> [a]
primeFactors n = [x | x <- factors n, isPrime x]
-- filter isPrime (factors n)

-- A list of primes up to n.
primesUpTo :: Integral a => a -> [a]
primesUpTo n = [x | x <- [2..n], isPrime x]
-- filter isPrime[2..n]

-- True iff n is a perfect number.
-- A number n is perfect if the sum of its factors is 2*n.
isPerfect :: Integral a => a -> Bool
isPerfect n = sum (factors n) == 2*n
-- isPerfect 0 = False
-- isPerfect n = (2*n) == sum(factors n)

-- A list of all perfect numbers up to n.
perfectUpTo :: Integral a => a -> [a]
perfectUpTo n = [x | x <- [1..n], isPerfect x]
-- filter isPerfect [6..n]

-- The next prime greater than n.
nextPrime :: Integral a => a -> a
nextPrime n | isPrime (n + 1) = n + 1
            | otherwise = nextPrime (n+1)

-- A list of the first n primes.
generatePrimes :: Integral a => a -> [a]
generatePrimes n = take (fromIntegral n) [x | x <- [2..], isPrime x]
-- take (fromIntegral n) $ filter isPrime [2..]

