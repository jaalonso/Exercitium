-- Suma_de_numeros_amigos_menores_que_n.hs
-- Suma de los números amigos menores que n
-- José A. Alonso Jiménez
-- Sevilla, 18-febrero-2022
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
--    sumaAmigosMenores :: Integer -> Integer
-- tal que (sumaAmigosMenores n) es la suma de los números amigos
-- menores que n. Por ejemplo,
--    sumaAmigosMenores 2000   == 2898
--    sumaAmigosMenores (10^5) == 852810
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Suma_de_numeros_amigos_menores_que_n where

import Data.List (genericLength, group, inits, nub, sort, subsequences)
import Data.Numbers.Primes (primeFactors)

-- 1ª solución                                                   --
-- ===========

sumaAmigosMenores1 :: Integer -> Integer
sumaAmigosMenores1 n =
  sum [x+y | (x,y) <- amigosMenores1 n]

-- (amigosMenores1 n) es la lista de los pares de números amigos (con la
-- primera componente menor que la segunda) que son menores que n. Por
-- ejemplo,
--    amigosMenores1 2000  ==  [(220,284),(1184,1210)]
amigosMenores1 :: Integer -> [(Integer,Integer)]
amigosMenores1 n =
  takeWhile (\(_,y) -> y < n) sucesionAmigos1

sucesionAmigos1 :: [(Integer,Integer)]
sucesionAmigos1 =
  [(x,y) | x <- [1..],
           let y = sumaDivisoresPropios1 x,
           y > x,
           sumaDivisoresPropios1 y == x]

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

sumaAmigosMenores2 :: Integer -> Integer
sumaAmigosMenores2 n =
  sum [x+y | (x,y) <- amigosMenores2 n]

amigosMenores2 :: Integer -> [(Integer,Integer)]
amigosMenores2 n =
  takeWhile (\(_,y) -> y < n) sucesionAmigos2

sucesionAmigos2 :: [(Integer,Integer)]
sucesionAmigos2 =
  [(x,y) | x <- [1..],
           let y = sumaDivisoresPropios2 x,
           y > x,
           sumaDivisoresPropios2 y == x]

sumaDivisoresPropios2 :: Integer -> Integer
sumaDivisoresPropios2 = sum . divisoresPropios2

divisoresPropios2 :: Integer -> [Integer]
divisoresPropios2 x = filter ((== 0) . mod x) [1..x-1]

-- 3ª solución                                                   --
-- ===========

sumaAmigosMenores3 :: Integer -> Integer
sumaAmigosMenores3 n =
  sum [x+y | (x,y) <- amigosMenores3 n]

amigosMenores3 :: Integer -> [(Integer,Integer)]
amigosMenores3 n =
  takeWhile (\(_,y) -> y < n) sucesionAmigos3

sucesionAmigos3 :: [(Integer,Integer)]
sucesionAmigos3 =
  [(x,y) | x <- [1..],
           let y = sumaDivisoresPropios3 x,
           y > x,
           sumaDivisoresPropios3 y == x]

sumaDivisoresPropios3 :: Integer -> Integer
sumaDivisoresPropios3 = sum . divisoresPropios3

divisoresPropios3 :: Integer -> [Integer]
divisoresPropios3 =
  init . nub . sort . map product . subsequences . primeFactors

-- 4ª solución                                                   --
-- ===========

sumaAmigosMenores4 :: Integer -> Integer
sumaAmigosMenores4 n =
  sum [x+y | (x,y) <- amigosMenores4 n]

amigosMenores4 :: Integer -> [(Integer,Integer)]
amigosMenores4 n =
  takeWhile (\(_,y) -> y < n) sucesionAmigos4

sucesionAmigos4 :: [(Integer,Integer)]
sucesionAmigos4 =
  [(x,y) | x <- [1..],
           let y = sumaDivisoresPropios4 x,
           y > x,
           sumaDivisoresPropios4 y == x]

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

sumaAmigosMenores5 :: Integer -> Integer
sumaAmigosMenores5 n =
  sum [x+y | (x,y) <- amigosMenores5 n]

amigosMenores5 :: Integer -> [(Integer,Integer)]
amigosMenores5 n =
  takeWhile (\(_,y) -> y < n) sucesionAmigos5

sucesionAmigos5 :: [(Integer,Integer)]
sucesionAmigos5 =
  [(x,y) | x <- [1..],
           let y = sumaDivisoresPropios5 x,
           y > x,
           sumaDivisoresPropios5 y == x]

sumaDivisoresPropios5 :: Integer -> Integer
sumaDivisoresPropios5 = sum . divisoresPropios5

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

sumaAmigosMenores6 :: Integer -> Integer
sumaAmigosMenores6 n =
  sum [x+y | (x,y) <- amigosMenores6 n]

amigosMenores6 :: Integer -> [(Integer,Integer)]
amigosMenores6 n =
  takeWhile (\(_,y) -> y < n) sucesionAmigos6

sucesionAmigos6 :: [(Integer,Integer)]
sucesionAmigos6 =
  [(x,y) | x <- [1..],
           let y = sumaDivisoresPropios6 x,
           y > x,
           sumaDivisoresPropios6 y == x]

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

sumaAmigosMenores7 :: Integer -> Integer
sumaAmigosMenores7 n =
  sum [x+y | (x,y) <- amigosMenores7 n]

amigosMenores7 :: Integer -> [(Integer,Integer)]
amigosMenores7 n =
  takeWhile (\(_,y) -> y < n) sucesionAmigos7

sucesionAmigos7 :: [(Integer,Integer)]
sucesionAmigos7 =
  [(x,y) | x <- [1..],
           let y = sumaDivisoresPropios7 x,
           y > x,
           sumaDivisoresPropios7 y == x]

-- Si la descomposición de x en factores primos es
--    x = p(1)^e(1) . p(2)^e(2) . .... . p(n)^e(n)
-- entonces la suma de los divisores de x es
--    p(1)^(e(1)+1) - 1     p(2)^(e(2)+1) - 1       p(n)^(e(2)+1) - 1
--   ------------------- . ------------------- ... -------------------
--        p(1)-1                p(2)-1                  p(n)-1
-- Ver la demostración en http://bit.ly/2zUXZPc

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

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaAmigosMenores1 6000
--    19026
--    (10.37 secs, 5,261,392,352 bytes)
--    λ> sumaAmigosMenores2 6000
--    19026
--    (3.86 secs, 3,161,700,400 bytes)
--    λ> sumaAmigosMenores3 6000
--    19026
--    (0.15 secs, 308,520,248 bytes)
--    λ> sumaAmigosMenores4 6000
--    19026
--    (0.23 secs, 271,421,184 bytes)
--    λ> sumaAmigosMenores5 6000
--    19026
--    (0.13 secs, 230,042,112 bytes)
--    λ> sumaAmigosMenores6 6000
--    19026
--    (0.12 secs, 202,638,880 bytes)
--    λ> sumaAmigosMenores7 6000
--    19026
--    (0.13 secs, 159,022,448 bytes)
--
--    λ> sumaAmigosMenores3 (10^5)
--    852810
--    (4.83 secs, 10,726,377,728 bytes)
--    λ> sumaAmigosMenores4 (10^5)
--    852810
--    (4.79 secs, 7,832,234,120 bytes)
--    λ> sumaAmigosMenores5 (10^5)
--    852810
--    (2.79 secs, 6,837,118,464 bytes)
--    λ> sumaAmigosMenores6 (10^5)
--    852810
--    (2.39 secs, 6,229,730,472 bytes)
--    λ> sumaAmigosMenores7 (10^5)
--    852810
--    (2.65 secs, 5,170,949,168 bytes)
