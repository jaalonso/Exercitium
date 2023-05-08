-- Pol_Division_de_Ruffini_con_representacion_densa.hs
-- TAD de los polinomios: Regla de Ruffini con representación densa.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu)
-- definir la función
--    ruffiniDensa :: Int -> [Int] -> [Int]
-- tal que (ruffiniDensa r cs) es la lista de los coeficientes del
-- cociente junto con el rsto que resulta de aplicar la regla de Ruffini
-- para dividir el polinomio cuya representación densa es cs entre
-- x-r. Por ejemplo,
--    ruffiniDensa 2 [1,2,-1,-2] == [1,4,7,12]
--    ruffiniDensa 1 [1,2,-1,-2] == [1,3,2,0]
-- ya que
--      | 1  2  -1  -2           | 1  2  -1  -2
--    2 |    2   8  14         1 |    1   3   2
--    --+--------------        --+-------------
--      | 1  4   7  12           | 1  3   2   0
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Pol_Division_de_Ruffini_con_representacion_densa where

import Test.QuickCheck

-- 1ª solución
-- ===========

ruffiniDensa :: Int -> [Int] -> [Int]
ruffiniDensa _ [] = []
ruffiniDensa r p@(c:cs) =
  c : [x+r*y | (x,y) <- zip cs (ruffiniDensa r p)]

-- 2ª solución
-- ===========

ruffiniDensa2 :: Int -> [Int] -> [Int]
ruffiniDensa2 r =
  scanl1 (\s x -> s * r + x)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_ruffiniDensa :: Int -> [Int] -> Bool
prop_ruffiniDensa r cs =
  ruffiniDensa r cs == ruffiniDensa2 r cs

-- La comprobación es
--    λ> quickCheck prop_ruffiniDensa
--    +++ OK, passed 100 tests.
