import Data.List (genericLength, group)
import Data.Numbers.Primes (primeFactors)

amigos :: Integer -> Integer -> Bool
amigos x y = sumaDivisoresPropios x == y &&
              sumaDivisoresPropios y == x

sumaDivisoresPropios :: Integer -> Integer
sumaDivisoresPropios x =
  product [(p^(e+1)-1) `div` (p-1) | (p,e) <- factorizacion x] - x

factorizacion :: Integer -> [(Integer,Integer)]
factorizacion = map primeroYlongitud . group . primeFactors

primeroYlongitud :: [a] -> (a,Integer)
primeroYlongitud (x:xs) = (x, 1 + genericLength xs)
