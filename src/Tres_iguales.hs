-- Tres_iguales.hs
-- Tres iguales
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    tresIguales :: Int -> Int -> Int -> Bool
-- tal que (tresIguales x y z) se verifica si los elementos x, y y z son
-- iguales. Por ejemplo,
--    tresIguales 4 4 4  ==  True
--    tresIguales 4 3 4  ==  False
-- ---------------------------------------------------------------------

module Tres_iguales where

tresIguales :: Int -> Int -> Int -> Bool
tresIguales x y z = x == y && y == z
