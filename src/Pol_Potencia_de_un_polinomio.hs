-- Pol_Potencia_de_un_polinomio.hs
-- TAD de los polinomios: Potencia de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
-- tal que (potencia p n) es la potencia n-ésima del polinomio p. Por
-- ejemplo,
--    λ> ejPol = consPol 1 2 (consPol 0 3 polCero)
--    λ> ejPol
--    2*x + 3
--    λ> potencia ejPol 2
--    4*x^2 + 12*x + 9
--    λ> potencia ejPol 3
--    8*x^3 + 36*x^2 + 54*x + 27
-- ---------------------------------------------------------------------

module Pol_Potencia_de_un_polinomio where

import TAD.Polinomio (Polinomio, polCero, consPol)
import Pol_Producto_polinomios (multPol)
import Test.QuickCheck

-- 1ª solución
-- ===========

potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potencia _ 0 = polUnidad
potencia p n = multPol p (potencia p (n-1))

polUnidad :: (Num a, Eq a) => Polinomio a
polUnidad = consPol 0 1 polCero

-- 2ª solución
-- ===========

potencia2 :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potencia2 _ 0 = polUnidad
potencia2 p n
  | even n    = potencia2 (multPol p p) (n `div` 2)
  | otherwise = multPol p (potencia2 (multPol p p) ((n-1) `div` 2))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_potencia :: Polinomio Int -> NonNegative Int -> Bool
prop_potencia p (NonNegative n) =
  potencia p n == potencia2 p n

-- La comprobación es
--    λ> quickCheck prop_potencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> import TAD.Polinomio (grado)
--    λ> ejPol = consPol 1 2 (consPol 0 3 polCero)
--    λ> grado (potencia ejPol 1000)
--    1000
--    (4.57 secs, 2,409,900,720 bytes)
--    λ> grado (potencia2 ejPol 1000)
--    1000
--    (2.78 secs, 1,439,596,632 bytes)
