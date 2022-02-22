-- Numeros_amigos.hs
-- Números amigos
-- José A. Alonso Jiménez
-- Sevilla, 16-febrero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Dos [números amigos](https://bit.ly/36gSRHt) son dos números enteros
-- positivos distintos tales que la suma de los divisores propios de
-- cada uno es igual al otro. Los divisores propios de un número
-- incluyen la unidad pero no al propio número. Por ejemplo, los
-- divisores propios de 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y
-- 110. La suma de estos números equivale a 284. A su vez, los divisores
-- propios de 284 son 1, 2, 4, 71 y 142. Su suma equivale a 220. Por
-- tanto, 220 y 284 son amigos.
--
-- Definir la función
--    amigos :: Integer -> Integer -> Bool
-- tal que (amigos x y) se verifica si los números x e y son amigos. Por
-- ejemplo,
--    amigos 220 284 == True
--    amigos 220 23  == False
--    amigos 42262694537514864075544955198125 42405817271188606697466971841875 == True
-- ---------------------------------------------------------------------

module Numeros_amigos where

import Data.List (genericLength, group, inits, nub, sort, subsequences)
import Data.Numbers.Primes (primeFactors)

-- 1ª solución                                                   --
-- ===========

amigos1 :: Integer -> Integer -> Bool
amigos1 x y = sumaDivisoresPropios1 x == y &&
              sumaDivisoresPropios1 y == x

-- (sumaDivisoresPropios1 x) es la suma de los divisores propios de
-- x. Por ejemplo,
--    sumaDivisoresPropios1 220  ==  284
--    sumaDivisoresPropios1 284  ==  220
sumaDivisoresPropios1 :: Integer -> Integer
sumaDivisoresPropios1 = sum . divisoresPropios1

-- (divisoresPropios1 x) es la lista de los divisores propios de x. Por
-- ejemplo,
--    divisoresPropios1 220  ==  [1,2,4,5,10,11,20,22,44,55,110]
--    divisoresPropios1 284  ==  [1,2,4,71,142]
divisoresPropios1 :: Integer -> [Integer]
divisoresPropios1 x = [n | n <- [1..x-1], x `mod` n == 0]

-- 2ª solución                                                   --
-- ===========

amigos2 :: Integer -> Integer -> Bool
amigos2 x y = sumaDivisoresPropios2 x == y &&
              sumaDivisoresPropios2 y == x

sumaDivisoresPropios2 :: Integer -> Integer
sumaDivisoresPropios2 = sum . divisoresPropios2

divisoresPropios2 :: Integer -> [Integer]
divisoresPropios2 x = filter ((== 0) . mod x) [1..x-1]

-- 3ª solución                                                   --
-- ===========

amigos3 :: Integer -> Integer -> Bool
amigos3 x y = sumaDivisoresPropios3 x == y &&
              sumaDivisoresPropios3 y == x

sumaDivisoresPropios3 :: Integer -> Integer
sumaDivisoresPropios3 = sum . divisoresPropios3

divisoresPropios3 :: Integer -> [Integer]
divisoresPropios3 =
  init . nub . sort . map product . subsequences . primeFactors

-- 4ª solución                                                   --
-- ===========

amigos4 :: Integer -> Integer -> Bool
amigos4 x y = sumaDivisoresPropios4 x == y &&
              sumaDivisoresPropios4 y == x

sumaDivisoresPropios4 :: Integer -> Integer
sumaDivisoresPropios4 = sum . divisoresPropios4

divisoresPropios4 :: Integer -> [Integer]
divisoresPropios4 =
  init
  . sort
  . map (product . concat)
  . productoCartesiano
  . map inits
  . group
  . primeFactors

-- (productoCartesiano xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    λ> productoCartesiano [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
productoCartesiano :: [[a]] -> [[a]]
productoCartesiano []       = [[]]
productoCartesiano (xs:xss) =
  [x:ys | x <- xs, ys <- productoCartesiano xss]

-- 5ª solución                                                   --
-- ===========

amigos5 :: Integer -> Integer -> Bool
amigos5 x y = sumaDivisoresPropios5 x == y &&
              sumaDivisoresPropios5 y == x

sumaDivisoresPropios5 :: Integer -> Integer
sumaDivisoresPropios5 =
  sum . divisoresPropios5

divisoresPropios5 :: Integer -> [Integer]
divisoresPropios5 =
  init
  . sort
  . map (product . concat)
  . sequence
  . map inits
  . group
  . primeFactors

-- 6ª solución                                                   --
-- ===========

amigos6 :: Integer -> Integer -> Bool
amigos6 x y = sumaDivisoresPropios6 x == y &&
              sumaDivisoresPropios6 y == x

sumaDivisoresPropios6 :: Integer -> Integer
sumaDivisoresPropios6 =
  sum
  . init
  . map (product . concat)
  . sequence
  . map inits
  . group
  . primeFactors

-- 7ª solución                                                   --
-- ===========

amigos7 :: Integer -> Integer -> Bool
amigos7 x y = sumaDivisoresPropios7 x == y &&
              sumaDivisoresPropios7 y == x

-- Si la descomposición de x en factores primos es
--    x = p(1)^e(1) . p(2)^e(2) . .... . p(n)^e(n)
-- entonces la suma de los divisores de x es
--    p(1)^(e(1)+1) - 1     p(2)^(e(2)+1) - 1       p(n)^(e(2)+1) - 1
--   ------------------- . ------------------- ... -------------------
--        p(1)-1                p(2)-1                  p(n)-1
-- Ver la demostración en http://bit.ly/2zUXZPc

-- (sumaDivisoresPropios7 x) es la suma de los divisores propios de
-- x. Por ejemplo,
--    sumaDivisoresPropios7 220  ==  284
--    sumaDivisoresPropios7 284  ==  220
sumaDivisoresPropios7 :: Integer -> Integer
sumaDivisoresPropios7 x =
  product [(p^(e+1)-1) `div` (p-1) | (p,e) <- factorizacion x] - x

-- (factorizacion x) es la lista de las bases y exponentes de la
-- descomposición prima de x. Por ejemplo,
--    factorizacion 600  ==  [(2,3),(3,1),(5,2)]
factorizacion :: Integer -> [(Integer,Integer)]
factorizacion = map primeroYlongitud . group . primeFactors

-- (primeroYlongitud xs) es el par formado por el primer elemento de xs
-- y la longitud de xs. Por ejemplo,
--    primeroYlongitud [3,2,5,7] == (3,4)
primeroYlongitud :: [a] -> (a,Integer)
primeroYlongitud (x:xs) = (x, 1 + genericLength xs)
primeroYlongitud [] = error "Imposible"

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> amigos1 2803580 3716164
--    True
--    (2.27 secs, 1,304,055,864 bytes)
--    λ> amigos2 2803580 3716164
--    True
--    (0.81 secs, 782,478,584 bytes)
--    λ> amigos3 2803580 3716164
--    True
--    (0.01 secs, 383,888 bytes)
--    λ> amigos4 2803580 3716164
--    True
--    (0.01 secs, 461,376 bytes)
--    λ> amigos5 2803580 3716164
--    True
--    (0.00 secs, 412,560 bytes)
--    λ> amigos6 2803580 3716164
--    True
--    (0.00 secs, 387,816 bytes)
--    λ> amigos7 2803580 3716164
--    True
--    (0.01 secs, 339,008 bytes)
--
--    λ> amigos2 5864660 7489324
--    True
--    (1.74 secs, 1,602,582,592 bytes)
--    λ> amigos3 5864660 7489324
--    True
--    (0.00 secs, 277,056 bytes)
--    λ> amigos4 5864660 7489324
--    True
--    (0.01 secs, 354,872 bytes)
--    λ> amigos5 5864660 7489324
--    True
--    (0.01 secs, 305,792 bytes)
--    λ> amigos6 5864660 7489324
--    True
--    (0.00 secs, 281,528 bytes)
--    λ> amigos7 5864660 7489324
--    True
--    (0.01 secs, 237,176 bytes)
--
--    λ> amigos3 42262694537514864075544955198125 42405817271188606697466971841875
--    True
--    (107.54 secs, 5,594,306,392 bytes)
--    λ> amigos4 42262694537514864075544955198125 42405817271188606697466971841875
--    True
--    (1.03 secs, 942,530,824 bytes)
--    λ> amigos5 42262694537514864075544955198125 42405817271188606697466971841875
--    True
--    (0.51 secs, 591,144,304 bytes)
--    λ> amigos6 42262694537514864075544955198125 42405817271188606697466971841875
--    True
--    (0.26 secs, 379,534,608 bytes)
--    λ> amigos7 42262694537514864075544955198125 42405817271188606697466971841875
--    True
--    (0.05 secs, 25,635,464 bytes)
