-- Parejas_de_numeros_y_divisores.hs
-- Parejas de números y divisores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisoresHasta :: Int -> [(Int,Int)]
-- tal que (divisoresHasta n) es la lista de los pares (a,b) tales que a
-- es un número entre 2 y n y b es un divisor propio de a. Por ejemplo,
--    λ> divisoresHasta 6
--    [(2,1),(3,1),(4,1),(5,1),(6,1),(4,2),(6,2),(6,3)]
--    λ> divisoresHasta 8
--    [(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(4,2),(6,2),(8,2),(6,3),(8,4)]
--    λ> length (divisoresHasta 1234567)
--    16272448
-- ---------------------------------------------------------------------

module Parejas_de_numeros_y_divisores where

import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Test.QuickCheck

-- 1ª solución
-- ===========

divisoresHasta1 :: Int -> [(Int,Int)]
divisoresHasta1 n =
  ordena (concat [[(a,b) | b <- divisoresPropios a] | a <- [2..n]])
  where ordena ps = [intercambia p | p <- sort (map intercambia ps)]
        intercambia (x,y) = (y,x)

divisoresPropios :: Int -> [Int]
divisoresPropios n = [x | x <- [1..n `div` 2], n `mod` x == 0]

-- 2ª solución
-- ===========

divisoresHasta2 :: Int -> [(Int,Int)]
divisoresHasta2 n =
  ordena (concat [[(a,b) | b <- divisoresPropios a] | a <- [2..n]])
  where ordena           = sortBy comp
        comp (x,y) (u,v) = compare (y,x) (v,u)

-- 3ª solución
-- ===========

divisoresHasta3 :: Int -> [(Int,Int)]
divisoresHasta3 n =
  ordena (concat [[(a,b) | b <- divisoresPropios a] | a <- [2..n]])
  where ordena = sortBy (comparing swap)

-- 4ª solución
-- ===========

divisoresHasta4 :: Int -> [(Int,Int)]
divisoresHasta4 n =
  [(a,b) | b <- [1..n `div` 2], a <- [2*b, 3*b..n]]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_divisoresHasta :: Positive Int -> Bool
prop_divisoresHasta (Positive n) =
  all (== divisoresHasta1 n)
      [ divisoresHasta2 n
      , divisoresHasta3 n
      , divisoresHasta4 n
      ]

-- La comprobación es
--    λ> quickCheck prop_divisoresHasta
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (divisoresHasta1 4000)
--    29805
--    (1.78 secs, 843,584,472 bytes)
--    λ> length (divisoresHasta2 4000)
--    29805
--    (1.78 secs, 879,421,056 bytes)
--    λ> length (divisoresHasta3 4000)
--    29805
--    (1.70 secs, 876,475,584 bytes)
--    λ> length (divisoresHasta4 4000)
--    29805
--    (0.02 secs, 6,332,016 bytes)
