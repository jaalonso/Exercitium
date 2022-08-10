-- Tres_diferentes.hs
-- Tres diferentes
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    tresDiferentes :: Int -> Int -> Int -> Bool
-- tal que (tresDiferentes x y z) se verifica si los elementos x, y y z
-- son distintos. Por ejemplo,
--    tresDiferentes 3 5 2  ==  True
--    tresDiferentes 3 5 3  ==  False
-- ---------------------------------------------------------------------

module Tres_diferentes where

tresDiferentes :: Int -> Int -> Int -> Bool
tresDiferentes x y z = x /= y && x /= z && y /= z
