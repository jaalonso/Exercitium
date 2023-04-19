-- Pol_Suma_de_polinomios.hs
-- TAD de los polinomios: Suma de polinomios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
-- (sumaPol p q) es la suma de los polinomios p y q. Por ejemplo,
--    λ> ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
--    λ> ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> ejPol1
--    3*x^4 + -5*x^2 + 3
--    λ> ejPol2
--    x^5 + 5*x^2 + 4*x
--    λ> sumaPol ejPol1 ejPol2
--    x^5 + 3*x^4 + 4*x + 3
--
-- Comprobar con QuickCheck las siguientes propiedades:
-- + polCero es el elemento neutro de la suma.
-- + la suma es conmutativa.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Suma_de_polinomios where

import TAD.Polinomio (Polinomio, polCero, esPolCero, consPol, grado,
                      coefLider, restoPol)
import Test.QuickCheck

sumaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
sumaPol p q
  | esPolCero p = q
  | esPolCero q = p
  | n1 > n2      = consPol n1 a1 (sumaPol r1 q)
  | n1 < n2      = consPol n2 a2 (sumaPol p r2)
  | otherwise    = consPol n1 (a1+a2) (sumaPol r1 r2)
  where (n1, a1, r1) = (grado p, coefLider p, restoPol p)
        (n2, a2, r2) = (grado q, coefLider q, restoPol q)

-- Propiedad. El polinomio cero es el elemento neutro de la suma.
prop_neutroSumaPol :: Polinomio Int -> Bool
prop_neutroSumaPol p =
  sumaPol polCero p == p

-- Comprobación con QuickCheck.
--    λ> quickCheck prop_neutroSumaPol
--    OK, passed 100 tests.

-- Propiedad. La suma es conmutativa.
prop_conmutativaSuma :: Polinomio Int -> Polinomio Int -> Bool
prop_conmutativaSuma p q =
  sumaPol p q == sumaPol q p

-- Comprobación:
--    λ> quickCheck prop_conmutativaSuma
--    OK, passed 100 tests.
