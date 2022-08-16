-- Elementos_finales.hs
-- Elementos finales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    finales :: Int -> [a] -> [a]
-- tal que (finales n xs) es la lista formada por los n finales
-- elementos de xs. Por ejemplo,
--    finales 3 [2,5,4,7,9,6]  ==  [7,9,6]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Elementos_finales where
import Test.QuickCheck

-- 1ª definición
finales1 :: Int -> [a] -> [a]
finales1 n xs = drop (length xs - n) xs

-- 2ª definición
finales2 :: Int -> [a] -> [a]
finales2 n xs = reverse (take n (reverse xs))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_finales :: Int -> [Int] -> Bool
prop_finales n xs =
  finales1 n xs == finales2 n xs

-- La comprobación es
--    λ> quickCheck prop_finales
--    +++ OK, passed 100 tests.
