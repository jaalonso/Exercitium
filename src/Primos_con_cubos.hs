-- Primos_con_cubos.hs
-- Primos con cubos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-febrero-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un **primo con cubo** es un número primo p para el que existe algún
-- entero positivo n tal que la expresión n^3 + n^2p es un cubo
-- perfecto. Por ejemplo, 19 es un primo con cubo ya que
-- 8^3 + 8^2×19 = 12^3.
--
-- Definir la sucesión
--    primosConCubos :: [Integer]
-- tal que sus elementos son los primos con cubo. Por ejemplo,
--    λ> take 6 primosConCubos
--    [7,19,37,61,127,271]
--    λ> length (takeWhile (<1000000) primosConCubos)
--    173
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Primos_con_cubos where

import Data.Numbers.Primes (isPrime)
import Test.QuickCheck (NonNegative (NonNegative), maxSize, quickCheckWith, stdArgs)

-- 1ª solución
-- ===========

primosConCubos1 :: [Integer]
primosConCubos1 =
  [p | x <- [1..],
       n <- [1..x],
       (x^3 - n^3) `mod` (n^2) == 0,
       let p = (x^3 - n^3) `div` (n^2),
       isPrime p]

-- 2ª solución
-- ===========

-- Para analizar la respuesta, en esta solución se calculan los pares
-- (p,n) tales que p es un primo con cubo y n es un número positivo tal
-- que n^3 + n^2p es un cubo

primosConCubos2' :: [(Integer,Integer)]
primosConCubos2' =
  [(p,n) | x <- [1..],
           n <- [1..x],
           (x^3 - n^3) `mod` (n^2) == 0,
           let p = (x^3 - n^3) `div` (n^2),
           isPrime p]

-- El cálculo es
--    λ> take 7 primosConCubos2
--    [(7,1),(19,8),(37,27),(61,64),(127,216),(271,729),(331,1000)]

-- Se observa que la sucesión de los segundos elementos [1,8,27,64,...]
-- es la de los cubos y que los primeros elementos se obtienen restando
-- los segundos elementos consecutivos; es decir,
--     7 =  8 -  1 = 2^3 - 1^3
--    19 = 27 -  8 = 3^3 - 2^3
--    37 = 64 - 27 = 4^3 - 3^3
-- Continuando el patrón,
--     61 =  5^3 - 4^3 =  125 -   64
--     91 =  6^3 - 5^3 =  216 -  125
--    127 =  7^3 - 6^3 =  343 -  216
--    271 = 10^3 - 9^3 = 1000 -  729
--    331 = 11^3 -10^3 = 1331 - 1000
-- Por tanto, los primos con cubos son diferencias de dos cubos
-- consecutivos; es decir, coinciden con los números cubanos del
-- ejercicio anterior. A partir de la conjetura se obtienen las
-- siguientes definiciones

-- Basado en las anteriores observaciones se obtiene la siguiente
-- definición
primosConCubos2 :: [Integer]
primosConCubos2 =
  filter isPrime [(x+1)^3 - x^3 | x <- [1..]]

-- 3ª definición
-- =============

primosConCubos3 :: [Integer]
primosConCubos3 =
  filter isPrime diferenciasCubosConsecutivos

diferenciasCubosConsecutivos :: [Integer]
diferenciasCubosConsecutivos =
  zipWith (-) (tail cubos) cubos

cubos :: [Integer]
cubos = map (^3) [0..]

-- 4ª solución
-- ===========

-- Simplificando la expresión (x+1)^3 - x^3 se obtiene 3*x^2+ 3*x + 1,
-- con lo que la 3ª definición se reduce a

primosConCubos4 :: [Integer]
primosConCubos4 =
  [p | x <- [1..],
       let p = 3*x^2+ 3*x + 1,
       isPrime p]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_primosConCubos :: NonNegative Int -> Bool
prop_primosConCubos (NonNegative n) =
  all (== primosConCubos1 !! n)
      [primosConCubos2 !! n,
       primosConCubos3 !! n,
       primosConCubos4 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_primosConCubos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> primosConCubos1 !! 7
--    397
--    (1.96 secs, 1,909,369,592 bytes)
--    λ> primosConCubos2 !! 7
--    397
--    (0.01 secs, 648,840 bytes)
--
--    λ> primosConCubos2 !! (10^3)
--    65580901
--    (0.53 secs, 1,726,837,688 bytes)
--    λ> primosConCubos3 !! (10^3)
--    65580901
--    (0.49 secs, 1,724,258,632 bytes)
--    λ> primosConCubos4 !! (10^3)
--    65580901
--    (0.47 secs, 1,724,833,992 bytes)

-- Demostración de la conjetura
-- ============================

-- Vamos a demostrar que los primos con cubos son diferencias de dos
-- cubos consecutivos.
--
-- Sea p un primo con cubo. Por la definición, existe un entero
-- positivo n tal que n³ + n²p es un cubo.
--
-- Lema 1: Los números n y p son coprimos (es decir, mcd(n,p) = 1).
-- Dem.: En caso contrario, puesto que p es primo, existe un a tal que
-- n = ap. Luego n³ + n²p = (a³+a²)p³ es un cubo y, por tanto,
-- a³+a² es un cubo lo que es imposible ya que el siguiente cubo de
-- a³ es a³+3a²+3a+1.
--
-- Lema 2: Los números n² y n+p son coprimos.
-- Dem.: Sea k = mcd(n^2,n+p). Por k divide n², luego k divide a n;
-- además, k divide a n+p y (usando el lema 1 y el ser p primo), se
-- tiene que k = 1.
--
-- Puesto que n³+n²p = n²(n+p) es un cubo, usando el lema 2, se tiene
-- que n² y n+p son cubos y, por serlo n², n también es un cubo. Es
-- decir, existen enteros positivos x e y tales que n = x³ y
-- n+p = y³. Por tanto, p = y³-x³. Sea k = y-x. Se tiene que k = 1 ya
-- que
--    p = y³-x³
--      = (n+k)³-n³
--      = 3k+3k²+k³
-- no es primo para k > 1.
--
-- Por consiguiente, p = (x+1)³-x³.

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- + OEIS [A002407](http://oeis.org/A002407).
