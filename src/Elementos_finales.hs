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

module Elementos_finales where

finales :: Int -> [a] -> [a]
finales n xs = drop (length xs - n) xs
