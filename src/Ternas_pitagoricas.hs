-- Ternas_pitagoricas.hs
-- Ternas pitagóricas
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una terna (x,y,z) de enteros positivos es pitagórica si x^2 + y^2 =
-- z^2 y x < y < z.
--
-- Definir, por comprensión, la función
--    pitagoricas :: Int -> [(Int,Int,Int)]
-- tal que (pitagoricas n) es la lista de todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n. Por ejemplo,
--    pitagoricas 10  ==  [(3,4,5),(6,8,10)]
--    pitagoricas 15  ==  [(3,4,5),(5,12,13),(6,8,10),(9,12,15)]
-- ---------------------------------------------------------------------

module Ternas_pitagoricas where

import Test.QuickCheck

-- 1ª solución
-- ===========

pitagoricas1 :: Int -> [(Int,Int,Int)]
pitagoricas1 n = [(x,y,z) | x <- [1..n]
                          , y <- [1..n]
                          , z <- [1..n]
                          , x^2 + y^2 == z^2
                          , x < y && y < z]

-- 2ª solución
-- ===========

pitagoricas2 :: Int -> [(Int,Int,Int)]
pitagoricas2 n = [(x,y,z) | x <- [1..n]
                          , y <- [x+1..n]
                          , z <- [ceiling (sqrt (fromIntegral (x^2+y^2)))..n]
                          , x^2 + y^2 == z^2]

-- 3ª solución
-- ===========

pitagoricas3 :: Int -> [(Int,Int,Int)]
pitagoricas3 n = [(x,y,z) | x <- [1..n]
                          , y <- [x+1..n]
                          , let z = round (sqrt (fromIntegral (x^2+y^2)))
                          , y < z
                          , z <= n
                          , x^2 + y^2 == z^2]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_pitagoricas :: Positive Int -> Bool
prop_pitagoricas (Positive n) =
  all (== pitagoricas1 n)
      [pitagoricas2 n,
       pitagoricas3 n]

-- La comprobación es
--    λ> quickCheck prop_pitagoricas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (pitagoricas1 200)
--    127
--    (12.25 secs, 12,680,320,400 bytes)
--    λ> length (pitagoricas2 200)
--    127
--    (1.61 secs, 1,679,376,824 bytes)
--    λ> length (pitagoricas3 200)
--    127
--    (0.06 secs, 55,837,072 bytes)
