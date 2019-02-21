primes' :: [Integer]
primes' = 
    2 : [p | p <- [3..], 
             all (\x -> mod p x /= 0) (takeWhile (\x -> x*x <= p) primes')]
