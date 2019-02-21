f :: [Integer] -> [Integer]
f [] = []
f (x:xs) = [k | k <- xs, mod k x /= 0]

primes :: [Integer]
primes = map head (iterate f [2..])