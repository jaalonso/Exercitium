-- Primeros_y_ultimos_elementos.hs
-- Primeros y últimos elementos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    extremos :: Int -> [a] -> [a]
-- tal que (extremos n xs) es la lista formada por los n primeros
-- elementos de xs y los n finales elementos de xs. Por ejemplo,
--    extremos 3 [2,6,7,1,2,4,5,8,9,2,3]  ==  [2,6,7,9,2,3]
-- ---------------------------------------------------------------------

module Primeros_y_ultimos_elementos where

extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs
