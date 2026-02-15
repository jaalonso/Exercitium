-- Densidad_de_numeros_abundantes.hs
-- Densidades de números abundantes, perfectos y deficientes.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-Febrero-2015 (actualizado 15-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La n-ésima densidad de un tipo de número es el cociente entre la
-- cantidad de los números entre 1 y n que son del tipo considerado
-- y n. Por ejemplo, la 7-ésima densidad de los múltiplos de 3 es 2/7 ya
-- que entre los 7 primeros números sólo 2 son múltiplos de 3.
--
-- Definir las funciones
--    densidades :: Int -> (Double,Double,Double)
--    graficas   :: Int -> IO ()
-- tales que
-- + (densidades n) es la terna formada por la n-ésima densidad de
--   los números [abundantes](http://bit.ly/1BniqiY) (es decir, para los
--   que la suma de sus divisores propios es mayor que el número), de los
--   números [perfectos](http://bit.ly/1BniShk) (es decir, para los
--   que la suma de sus divisores propios igual al número) y de los
--   números deficientes[http://bit.ly/1BniQ9h] (es decir, para los
--   que la suma de sus divisores propios es menor que el número). Por
--   ejemplo,
--      densidades 100     ==  (0.22,    2.0e-2, 0.76)
--      densidades 1000    ==  (0.246,   3.0e-3, 0.751)
--      densidades 10000   ==  (0.2488,  4.0e-4, 0.7508)
--      densidades 100000  ==  (0.24795, 4.0e-5, 0.75201)
--      densidades 1000000 ==  (0.247545,4.0e-6, 0.752451)
--
-- + (graficas n) dibuja las gráficas de las k-ésimas densidades (para k
--   entre 1 y n) de los números abundantes, de los números perfectos y
--   de los números deficientes. Por ejemplo, (graficas 100) dibuja
--      Densidad_de_numeros_abundantes1.png
--   y (graficas 400) dibuja
--      Densidad_de_numeros_abundantes2.png
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Densidad_de_numeros_abundantes where

import Data.List (genericLength, group, partition)
import Data.Array (accumArray, assocs)
import Data.Numbers.Primes (primeFactors)
import Math.NumberTheory.ArithmeticFunctions (sigma)
import Graphics.Gnuplot.Simple (plotLists, Attribute (Key))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Fuerza bruta
-- =========================

densidades1 :: Int -> (Double,Double,Double)
densidades1 n = (f a, f p, f d)
  where a   = nAbundantes n
        p   = nPerfectos n
        d   = n - a - p
        f x = fromIntegral x / fromIntegral n

-- (nAbundantes n) es la cantidad de números abundantes desde 1 hasta
-- n. Por ejemplo,
--    nAbundantes 100 == 22
nAbundantes :: Int -> Int
nAbundantes n = length (filter esAbundante [1..n])

-- (esAbundante n) se verifica si n es un número abundante. Por ejemplo,
--    esAbundante 12 == True
--    esAbundante 22 == False
esAbundante :: Int -> Bool
esAbundante n = sumaDivisores n > n

-- (sumaDivisores n) es la suma de los divisores propios de n. Por
-- ejemplo,
--    sumaDivisores 12 == 16
--    sumaDivisores 22 == 14
sumaDivisores :: Int -> Int
sumaDivisores = sum . divisores

-- (divisores n) es la lista de los divisores propios de n. Por ejemplo,
--    divisores 12== [1,2,3,4,6]
--    divisores 22== [1,2,11]
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n-1],
                   n `mod` x == 0]

-- (nPerfectos n) es la cantidad de números perfectos desde 1 hasta
-- n. Por ejemplo,
--    nPerfectos 100 == 2
nPerfectos :: Int -> Int
nPerfectos n = length (filter esPerfecto [1..n])

-- (esPerfecto n) se verifica si n es un número perfecto. Por ejemplo,
--    esPerfecto 28 == True
--    esPerfecto 38 == False
esPerfecto :: Int -> Bool
esPerfecto n = sumaDivisores n == n

-- 2ª solución: Uso de partición
-- =============================

densidades2 :: Int -> (Double,Double,Double)
densidades2 n = (f as, f ps, f ds)
  where (as,pds) = partition esAbundante [1..n]
        (ps,ds)  = partition esPerfecto pds
        f xs     = genericLength xs / fromIntegral n

-- 3ª solución: Modelado con tipos de datos
-- ========================================

densidades3 :: Int -> (Double,Double,Double)
densidades3 n = (f as, f ps, f ds)
  where cs       = map clasificacion [1..n]
        (as,pds) = partition (== Abundante) cs
        (ps,ds)  = partition (== Perfecto) pds
        f xs     = genericLength xs / fromIntegral n

data Clase = Abundante | Perfecto | Deficiente
  deriving (Eq, Show)

-- (clasificacion n) es la clase de número de n. Por ejemplo,
--    clasificacion 12 == Abundante
--    clasificacion 22 == Deficiente
--    clasificacion 28 == Perfecto
clasificacion :: Int -> Clase
clasificacion n
  | sd > n    = Abundante
  | sd < n    = Deficiente
  | otherwise = Perfecto
  where sd = sumaDivisores n

-- 4ª solución: Optimización por factorización prima
-- =================================================

densidades4 :: Int -> (Double,Double,Double)
densidades4 n = (f as, f ps, f ds)
  where cs       = map clasificacion2 [1..n]
        (as,pds) = partition (== Abundante) cs
        (ps,ds)  = partition (== Perfecto) pds
        f xs     = genericLength xs / fromIntegral n

-- 2ª definición de clasificacion
clasificacion2 :: Int -> Clase
clasificacion2 n
  | sd > n    = Abundante
  | sd < n    = Deficiente
  | otherwise = Perfecto
  where sd = sumaDivisores2 n

-- 2ª definición de sumaDivisores
sumaDivisores2 :: Int -> Int
sumaDivisores2 x =
  product [(p^(e+1)-1) `div` (p-1) | (p,e) <- factorizacion x] - x

-- (factorizacion x) es la lista de las bases y exponentes de la
-- descomposición prima de x. Por ejemplo,
--    factorizacion 600  ==  [(2,3),(3,1),(5,2)]
factorizacion :: Int -> [(Int,Int)]
factorizacion = map primeroYlongitud . group . primeFactors

-- (primeroYlongitud xs) es el par formado por el primer elemento de xs
-- y la longitud de xs. Por ejemplo,
--    primeroYlongitud [3,2,5,7] == (3,4)
primeroYlongitud :: [a] -> (a,Int)
primeroYlongitud (x:xs) =
  (x, 1 + length xs)

-- 5ª solución: Uso de librerías especializadas
-- ============================================

densidades5 :: Int -> (Double,Double,Double)
densidades5 n = (f as, f ps, f ds)
  where cs       = map clasificacion3 [1..n]
        (as,pds) = partition (== Abundante) cs
        (ps,ds)  = partition (== Perfecto) pds
        f xs     = genericLength xs / fromIntegral n

-- 2ª definición de clasificacion
clasificacion3 :: Int -> Clase
clasificacion3 n
  | sd > n    = Abundante
  | sd < n    = Deficiente
  | otherwise = Perfecto
  where sd = sumaDivisores3 n

-- 3ª definición de sumaDivisores
sumaDivisores3 :: Int -> Int
sumaDivisores3 x = sigma 1 x - x

-- 6ª solución: Algoritmo de criba
-- ===============================

densidades6 :: Int -> (Double,Double,Double)
densidades6 n = (f a, f p, f d)
  where (a,p,d) = distribucion n
        f x = fromIntegral x / fromIntegral n

-- (distribucion n) es la terna (a,p,d) donde a es la cantidad de
-- números abundantes de 1 a n, p la de los perfectos y d la de los
-- deficientes. Por ejemplo,
--    distribucion 100  ==  (22,2,76)
distribucion :: Int -> (Int,Int,Int)
distribucion n = aux (0,0,0) (sumaDivisoresHasta n)
  where aux (a,p,d) [] = (a,p,d)
        aux (a,p,d) ((x,y):xys)
          | x < y     = aux (1+a,p,d) xys
          | x > y     = aux (a,p,1+d) xys
          | otherwise = aux (a,1+p,d) xys

-- (sumaDivisoresHasta n) es la lista de los pares (a,b) tales que a
-- varía entre 1 y n y b es la suma de los divisores propios de a. Por
-- ejemplo,
--    λ> sumaDivisoresHasta 12
--    [(1,0),(2,1),(3,1),(4,3),(5,1),(6,6),(7,1),(8,7),(9,4),(10,8),(11,1),(12,16)]
sumaDivisoresHasta :: Int -> [(Int,Int)]
sumaDivisoresHasta n =
  assocs (accumArray (+) 0 (1,n) (divisoresHasta n))

-- (divisoresHasta n) es la lista de los pares (a,b) tales que a está
-- entre 2 y n y b es un divisor propio e x. Por ejemplo,
--    λ> divisoresHasta 6
--    [(2,1),(3,1),(4,1),(5,1),(6,1),(4,2),(6,2),(6,3)]
--    λ> divisoresHasta 8
--    [(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(4,2),(6,2),(8,2),(6,3),(8,4)]
divisoresHasta :: Int -> [(Int,Int)]
divisoresHasta n = [(a,b) | b <- [1..n `div` 2], a <- [b*2, b*3..n]]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> (Double,Double,Double)) -> Spec
specG densidades = do
  it "e1" $
    densidades 100 `shouldBe` (0.22,2.0e-2,0.76)
  it "e2" $
    densidades 200 `shouldBe` (0.23,1.0e-2,0.76)

spec :: Spec
spec = do
  describe "def. 1" $ specG densidades1
  describe "def. 2" $ specG densidades2
  describe "def. 3" $ specG densidades3
  describe "def. 4" $ specG densidades4
  describe "def. 5" $ specG densidades5
  describe "def. 6" $ specG densidades6

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> Bool
prop_equivalencia (Positive n) =
  all (== densidades1 n)
      [ densidades2 n
      , densidades3 n
      , densidades4 n
      , densidades5 n
      , densidades6 n
      ]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> densidades1 2000
--    (0.2465,1.5e-3,0.752)
--    (1.59 secs, 804,883,768 bytes)
--    λ> densidades2 2000
--    (0.2465,1.5e-3,0.752)
--    (1.39 secs, 704,749,552 bytes)
--    λ> densidades3 2000
--    (0.2465,1.5e-3,0.752)
--    (0.81 secs, 403,783,312 bytes)
--    λ> densidades4 2000
--    (0.2465,1.5e-3,0.752)
--    (0.04 secs, 34,364,960 bytes)
--    λ> densidades5 2000
--    (0.2465,1.5e-3,0.752)
--    (0.04 secs, 7,554,992 bytes)
--    λ> densidades6 2000
--    (0.2465,1.5e-3,0.752)
--    (0.03 secs, 4,803,560 bytes)
--
--    λ> densidades4 100000
--    (0.24795,4.0e-5,0.75201)
--    (2.08 secs, 4,545,320,016 bytes)
--    λ> densidades5 100000
--    (0.24795,4.0e-5,0.75201)
--    (0.28 secs, 384,251,344 bytes)
--    λ> densidades6 100000
--    (0.24795,4.0e-5,0.75201)
--    (0.52 secs, 292,076,344 bytes)
--
-- La comparación de (densidades 2000) es:
--    | Solución | Tiempo | Memoria |
--    |----------|--------|---------|
--    | 1ª       | 1.59 s | 804 MB  |
--    | 2ª       | 1.39 s | 704 MB  |
--    | 3ª       | 0.81 s | 403 MB  |
--    | 4ª       | 0.04 s |  34 MB  |
--    | 5ª       | 0.04 s |   7 MB  |
--    | 6ª       | 0.03 s |   5 MB  |
--
-- La comparación de (densidades 100000) es:
--    | Solución | Tiempo | Memoria |
--    |----------|--------|---------|
--    | 4ª       | 2.08 s | 4545 GB |
--    | 5ª       | 0.28 s |  384 MB |
--    | 6ª       | 0.52 s |  292 MB |

-- Gráfica
-- =======

graficas :: Int -> IO ()
graficas n =
  plotLists [Key Nothing]
            [ [x | (x,_,_) <- ts]
            , [y | (_,y,_) <- ts]
            , [z | (_,_,z) <- ts]]
  where ts = [densidades6 k | k <- [1..n]]
