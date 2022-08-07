-- Rango_de_una_lista.hs
-- Rango de una lista.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    rango :: [Int] -> [Int]
-- tal que (rango xs) es la lista formada por el menor y mayor elemento
-- de xs.
--    rango [3,2,7,5]  ==  [2,7]
-- ---------------------------------------------------------------------

module Rango_de_una_lista where

rango :: [Int] -> [Int]
rango xs = [minimum xs, maximum xs]
