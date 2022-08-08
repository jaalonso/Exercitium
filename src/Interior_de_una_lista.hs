-- Interior_de_una_lista.hs
-- Interior de una lista
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    interior :: [a] -> [a]
-- tal que (interior xs) es la lista obtenida eliminando los extremos de
-- la lista xs. Por ejemplo,
--    interior [2,5,3,7,3]  ==  [5,3,7]
--    interior [2..7]       ==  [3,4,5,6]
-- ---------------------------------------------------------------------

module Interior_de_una_lista where

interior :: [a] -> [a]
interior xs = tail (init xs)
