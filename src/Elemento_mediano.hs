-- Elemento_mediano.hs
-- Elemento mediano.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    mediano :: Int -> Int -> Int -> Int
-- tal que (mediano x y z) es el número mediano de los tres números x, y
-- y z. Por ejemplo,
--    mediano 3 2 5  ==  3
--    mediano 2 4 5  ==  4
--    mediano 2 6 5  ==  5
--    mediano 2 6 6  ==  6
-- ---------------------------------------------------------------------

module Elemento_mediano where

mediano :: Int -> Int -> Int -> Int
mediano x y z = x + y + z - minimum [x,y,z] - maximum [x,y,z]
