-- Suma_elementos_consecutivos.hs
-- Suma elementos consecutivos
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaConsecutivos :: [Integer] -> [Integer]
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaConsecutivos [3,1,5,2]  ==  [4,6,7]
--    sumaConsecutivos [3]        ==  []
--    last (sumaConsecutivos [1..10^8])  ==  199999999
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Suma_elementos_consecutivos where

import Test.QuickCheck

-- 1ª solución
-- ===========

sumaConsecutivos1 :: [Integer] -> [Integer]
sumaConsecutivos1 xs = [x+y | (x,y) <- zip xs (tail xs)]

-- 2ª solución
-- ===========

sumaConsecutivos2 :: [Integer] -> [Integer]
sumaConsecutivos2 xs = zipWith (+) xs (tail xs)

-- 3ª solución
-- ===========

sumaConsecutivos3 :: [Integer] -> [Integer]
sumaConsecutivos3 = zipWith (+) <*> tail

-- 4ª solución
-- ===========

sumaConsecutivos4 :: [Integer] -> [Integer]
sumaConsecutivos4 (x:y:zs) = x+y : sumaConsecutivos4 (y:zs)
sumaConsecutivos4 _        = []

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumaConsecutivos :: [Integer] -> Bool
prop_sumaConsecutivos xs =
  all (== sumaConsecutivos1 xs)
      [sumaConsecutivos2 xs,
       sumaConsecutivos3 xs,
       sumaConsecutivos4 xs]

-- La comprobación es
--    λ> quickCheck prop_sumaConsecutivos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (sumaConsecutivos1 [1..8*10^6])
--    15999999
--    (1.98 secs, 2,176,566,784 bytes)
--    λ> last (sumaConsecutivos2 [1..8*10^6])
--    15999999
--    (0.19 secs, 1,408,566,840 bytes)
--    λ> last (sumaConsecutivos3 [1..8*10^6])
--    15999999
--    (0.19 secs, 1,408,566,936 bytes)
--    λ> last (sumaConsecutivos4 [1..8*10^6])
--    15999999
--    (2.78 secs, 2,560,566,832 bytes)
