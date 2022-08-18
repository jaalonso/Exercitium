-- Permutacion_ciclica.hs
-- Permutación cíclica
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir una función
--    ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando cíclicamente los
-- elementos de la lista xs, pasando el último elemento al principio de
-- la lista. Por ejemplo,
--    ciclo [2,5,7,9]  == [9,2,5,7]
--    ciclo []         == []
--    ciclo [2]        == [2]
--
-- Comprobar que la longitud es un invariante de la función ciclo; es
-- decir, la longitud de (ciclo xs) es la misma que la de xs.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Permutacion_ciclica where

import Test.QuickCheck

ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = last xs : init xs

-- La propiedad es
prop_ciclo :: [Int] -> Bool
prop_ciclo xs = length (ciclo xs) == length xs

-- La comprobación es
--    λ> quickCheck prop_ciclo
--    +++ OK, passed 100 tests.
