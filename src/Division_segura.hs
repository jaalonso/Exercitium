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

module Division_segura where

-- 1ª definición
divisionSegura1 :: Double -> Double -> Double
divisionSegura1 x y =
  if y == 0 then 9999 else x/y

-- 2ª definición
divisionSegura2 :: Double -> Double -> Double
divisionSegura2 _ 0 = 9999
divisionSegura2 x y = x/y
