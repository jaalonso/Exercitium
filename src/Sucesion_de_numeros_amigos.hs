-- Sucesion_de_numeros_amigos.hs
-- Sucesión de números amigos
-- José A. Alonso Jiménez
-- Sevilla, 15-enero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Dos [números amigos](https://tinyurl.com/2y2ktgb9) son dos números
-- enteros  positivos distintos tales que la suma de los divisores
-- propios de cada uno es igual al otro. Los divisores propios de un
-- número incluyen la unidad pero no al propio número. Por ejemplo, los
-- divisores propios de 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y
-- 110. La suma de estos números equivale a 284. A su vez, los divisores
-- propios de 284 son 1, 2, 4, 71 y 142. Su suma equivale a 220. Por
-- tanto, 220 y 284 son amigos.
--
-- Definir la lista
--    sucesionAmigos :: [(Integer,Integer)]
-- cuyos elementos son los pares de números amigos con la primera
-- componente menor que la segunda. Por ejemplo,
--    take 4 sucesionAmigos == [(220,284),(1184,1210),(2620,2924),(5020,5564)]
--    sucesionAmigos6 !! 20 == (185368,203432)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Sucesion_de_numeros_amigos where

import Data.List (genericLength, group, inits, nub, sort, subsequences)
import Data.Numbers.Primes (primeFactors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución                                                   --
-- ===========

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
  . mapM inits
  . group
  . primeFactors

-- 7ª solución                                                   --
-- ===========

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

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [(Integer,Integer)] -> Spec
specG sucesionAmigos = do
  it "e1" $
    head sucesionAmigos `shouldBe` (220,284)

spec :: Spec
spec = do
  describe "def. 1" $ specG sucesionAmigos1
  describe "def. 2" $ specG sucesionAmigos2
  describe "def. 3" $ specG sucesionAmigos3
  describe "def. 4" $ specG sucesionAmigos4
  describe "def. 5" $ specG sucesionAmigos5
  describe "def. 6" $ specG sucesionAmigos6
  describe "def. 7" $ specG sucesionAmigos7

-- La verificación es
--    λ> verifica
--    7 examples, 0 failures

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> take 4 sucesionAmigos1
--    [(220,284),(1184,1210),(2620,2924),(5020,5564)]
--    (6.00 secs, 3,413,777,560 bytes)
--    λ> take 4 sucesionAmigos2
--    [(220,284),(1184,1210),(2620,2924),(5020,5564)]
--    (2.38 secs, 2,052,151,800 bytes)
--    λ> take 4 sucesionAmigos3
--    [(220,284),(1184,1210),(2620,2924),(5020,5564)]
--    (0.14 secs, 235,238,864 bytes)
--    λ> take 4 sucesionAmigos4
--    [(220,284),(1184,1210),(2620,2924),(5020,5564)]
--    (0.20 secs, 208,315,832 bytes)
--    λ> take 4 sucesionAmigos5
--    [(220,284),(1184,1210),(2620,2924),(5020,5564)]
--    (0.09 secs, 176,149,160 bytes)
--    λ> take 4 sucesionAmigos6
--    [(220,284),(1184,1210),(2620,2924),(5020,5564)]
--    (0.07 secs, 154,686,728 bytes)
--    λ> take 4 sucesionAmigos7
--    [(220,284),(1184,1210),(2620,2924),(5020,5564)]
--    (0.12 secs, 120,826,648 bytes)
--
--    λ> sucesionAmigos3 !! 10
--    (67095,71145)
--    (3.52 secs, 6,749,059,064 bytes)
--    λ> sucesionAmigos4 !! 10
--    (67095,71145)
--    (3.11 secs, 4,951,018,904 bytes)
--    λ> sucesionAmigos5 !! 10
--    (67095,71145)
--    (1.69 secs, 4,294,457,320 bytes)
--    λ> sucesionAmigos6 !! 10
--    (67095,71145)
--    (1.43 secs, 3,889,045,760 bytes)
--    λ> sucesionAmigos7 !! 10
--    (67095,71145)
--    (1.63 secs, 3,191,073,224 bytes)
--
--    λ> sucesionAmigos5 !! 12
--    (79750,88730)
--    (2.13 secs, 5,312,053,312 bytes)
--    λ> sucesionAmigos6 !! 12
--    (79750,88730)
--    (1.78 secs, 4,820,560,920 bytes)
--    λ> sucesionAmigos7 !! 12
--    (79750,88730)
--    (2.11 secs, 3,971,113,184 bytes)
