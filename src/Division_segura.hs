-- Division_segura.hs
-- División segura.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. Por ejemplo,
--    divisionSegura 7 2  ==  3.5
--    divisionSegura 7 0  ==  9999.0
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Division_segura where

import Test.QuickCheck

-- 1ª definición
divisionSegura1 :: Double -> Double -> Double
divisionSegura1 x y =
  if y == 0 then 9999 else x/y

-- 2ª definición
divisionSegura2 :: Double -> Double -> Double
divisionSegura2 _ 0 = 9999
divisionSegura2 x y = x/y

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_divisionSegura :: Double -> Double -> Bool
prop_divisionSegura x y =
  divisionSegura1 x y == divisionSegura2 x y

-- La comprobación es
--    λ> quickCheck prop_divisionSegura
--    +++ OK, passed 100 tests.
