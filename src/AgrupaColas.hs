-- AgrupaColas.hs
-- TAD de las colas: Agrupación de colas
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    agrupaColas :: [Cola a] -> Cola a
-- tal que (agrupaColas [c1,c2,c3,...,cn]) es la cola formada mezclando
-- las colas de la lista como sigue: mezcla c1 con c2, el resultado con
-- c3, el resultado con c4, y así sucesivamente. Por ejemplo,
--    λ> ej1 = inserta 2 (inserta 5 vacia)
--    λ> ej2 = inserta 3 (inserta 7 (inserta 4 vacia))
--    λ> ej3 = inserta 9 (inserta 0 (inserta 1 (inserta 6 vacia)))
--    λ> agrupaColas []
--    -
--    λ> agrupaColas [ej1]
--    5 | 2
--    λ> agrupaColas [ej1, ej2]
--    5 | 4 | 2 | 7 | 3
--    λ> agrupaColas [ej1, ej2, ej3]
--    5 | 6 | 4 | 1 | 2 | 0 | 7 | 9 | 3
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module AgrupaColas where

import TAD.Cola (Cola, vacia, inserta)
import IntercalaColas (intercalaColas)
import Test.QuickCheck

-- 1ª solución
-- ===========

agrupaColas1 :: [Cola a] -> Cola a
agrupaColas1 []            = vacia
agrupaColas1 [c]           = c
agrupaColas1 (c1:c2:colas) = agrupaColas1 ((intercalaColas c1 c2) : colas)

-- La función intercalaColas está definida en el ejercicio
-- "TAD de las colas: Intercalado de dos colas" que se encuentra en
-- https://bit.ly/3XYyjsM

-- 2ª solución
-- ===========

agrupaColas2 :: [Cola a] -> Cola a
agrupaColas2 = foldl intercalaColas vacia

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_agrupaColas :: [Cola Int] -> Bool
prop_agrupaColas cs =
  agrupaColas1 cs == agrupaColas2 cs

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=30}) prop_agrupaColas
--    +++ OK, passed 100 tests.
