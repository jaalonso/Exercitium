-- Iguales_al_siguiente.hs
-- Iguales al siguiente.
-- José A. Alonso Jiménez https://jaalonso.github.io
-- Sevilla, 21 de abril de 2014 (Revisión del 14 de febrero de 2022)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Definir la función
--    igualesAlSiguiente :: Eq a => [a] -> [a]
-- tal que (igualesAlSiguiente xs) es la lista de los elementos de xs
-- que son iguales a su siguiente. Por ejemplo,
--    igualesAlSiguiente [1,2,2,2,3,3,4]  ==  [2,2,3]
--    igualesAlSiguiente [1..10]          ==  []
-- ---------------------------------------------------------------------

module Iguales_al_siguiente where

import Data.List (group)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

igualesAlSiguiente1 :: Eq a => [a] -> [a]
igualesAlSiguiente1 xs =
  [x | (x, y) <- consecutivos1 xs, x == y]

-- (consecutivos1 xs) es la lista de pares de elementos consecutivos en
-- xs. Por ejemplo,
--    consecutivos1 [3,5,2,7]  ==  [(3,5),(5,2),(2,7)]
consecutivos1 :: [a] -> [(a, a)]
consecutivos1 xs = zip xs (tail xs)

-- 2ª solución
-- ===========

igualesAlSiguiente2 :: Eq a => [a] -> [a]
igualesAlSiguiente2 xs =
  [x | (x,y) <- consecutivos2 xs, x == y]

-- (consecutivos2 xs) es la lista de pares de elementos consecutivos en
-- xs. Por ejemplo,
--    consecutivos2 [3,5,2,7]  ==  [(3,5),(5,2),(2,7)]
consecutivos2 :: [a] -> [(a, a)]
consecutivos2 (x:y:zs) = (x,y) : consecutivos2 (y:zs)
consecutivos2 _        = []

-- 3ª solución
-- ===========

igualesAlSiguiente3 :: Eq a => [a] -> [a]
igualesAlSiguiente3 (x:y:zs) | x == y    = x : igualesAlSiguiente3 (y:zs)
                             | otherwise = igualesAlSiguiente3 (y:zs)
igualesAlSiguiente3 _                    = []

-- 4ª solución
-- ===========

igualesAlSiguiente4 :: Eq a => [a] -> [a]
igualesAlSiguiente4 xs = concat [ys | (_:ys) <- group xs]

-- 5ª solución
-- ===========

igualesAlSiguiente5 :: Eq a => [a] -> [a]
igualesAlSiguiente5 xs = concat (map tail (group xs))

-- 6ª solución
-- ===========

igualesAlSiguiente6 :: Eq a => [a] -> [a]
igualesAlSiguiente6 xs = tail =<< group xs

-- 7ª solución
-- ===========

igualesAlSiguiente7 :: Eq a => [a] -> [a]
igualesAlSiguiente7 = (tail =<<) . group

-- 8ª solución
-- ===========

igualesAlSiguiente8 :: Eq a => [a] -> [a]
igualesAlSiguiente8 xs = concatMap tail (group xs)

-- 9ª solución
-- ===========

igualesAlSiguiente9 :: Eq a => [a] -> [a]
igualesAlSiguiente9 = concatMap tail . group

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_igualesAlSiguiente :: [Int] -> Bool
prop_igualesAlSiguiente xs =
  all (== igualesAlSiguiente1 xs)
      [igualesAlSiguiente2 xs,
       igualesAlSiguiente3 xs,
       igualesAlSiguiente4 xs,
       igualesAlSiguiente5 xs,
       igualesAlSiguiente6 xs,
       igualesAlSiguiente7 xs,
       igualesAlSiguiente8 xs]

verificacion :: IO ()
verificacion = quickCheck prop_igualesAlSiguiente

-- La comprobación es
--    λ> verificacion
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    > ej = concatMap show [1..10^6]
--    (0.01 secs, 446,752 bytes)
--    λ> length ej
--    5888896
--    (0.16 secs, 669,787,856 bytes)
--    λ> length (show (igualesAlSiguiente1 ej))
--    588895
--    (1.60 secs, 886,142,944 bytes)
--    λ> length (show (igualesAlSiguiente2 ej))
--    588895
--    (1.95 secs, 1,734,143,816 bytes)
--    λ> length (show (igualesAlSiguiente3 ej))
--    588895
--    (1.81 secs, 1,178,232,104 bytes)
--    λ> length (show (igualesAlSiguiente4 ej))
--    588895
--    (1.43 secs, 1,932,010,304 bytes)
--    λ> length (show (igualesAlSiguiente5 ej))
--    588895
--    (0.40 secs, 2,016,810,320 bytes)
--    λ> length (show (igualesAlSiguiente6 ej))
--    588895
--    (0.32 secs, 1,550,409,984 bytes)
--    λ> length (show (igualesAlSiguiente7 ej))
--    588895
--    (0.34 secs, 1,550,410,104 bytes)
--    λ> length (show (igualesAlSiguiente8 ej))
--    588895
--    (0.33 secs, 1,550,410,024 bytes)
