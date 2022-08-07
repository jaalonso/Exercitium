-- Reconocimiento_de_palindromos.hs
-- Reconocimiento de palíndromos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-agosto-2022
-- ---------------------------------------------------------------------

module Reconocimiento_de_palindromos where

-- ---------------------------------------------------------------------
-- Definir la función
--    palindromo :: Eq a => [a] -> Bool
-- tal que (palindromo xs) se verifica si xs es un palíndromo; es decir,
-- es lo mismo leer xs de izquierda a derecha que de derecha a
-- izquierda. Por ejemplo,
--    palindromo [3,2,5,2,3]    ==  True
--    palindromo [3,2,5,6,2,3]  ==  False
-- ---------------------------------------------------------------------
