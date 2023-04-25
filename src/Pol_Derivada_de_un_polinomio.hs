-- Pol_Derivada_de_un_polinomio.hs
-- TAD de los polinomios: Derivada de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    derivada :: (Eq a, Num a) => Polinomio a -> Polinomio a
-- tal que (derivada p) es la derivada del polinomio p. Por ejemplo,
--    λ> ejPol = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> ejPol
--    x^5 + 5*x^2 + 4*x
--    λ> derivada ejPol
--    5*x^4 + 10*x + 4
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Derivada_de_un_polinomio where

import TAD.Polinomio (Polinomio, polCero, consPol, grado, coefLider,
                      restoPol)
import Pol_Suma_de_polinomios (sumaPol)
import Test.QuickCheck

derivada :: (Eq a, Num a) => Polinomio a -> Polinomio a
derivada p
  | n == 0     = polCero
  | otherwise  = consPol (n-1) (b * fromIntegral n) (derivada r)
  where n = grado p
        b = coefLider p
        r = restoPol p

-- Propiedad. La derivada de la suma es la suma de las derivadas.
prop_derivada :: Polinomio Int -> Polinomio Int -> Bool
prop_derivada p q =
  derivada (sumaPol p q) == sumaPol (derivada p) (derivada q)

-- Comprobación
--    λ> quickCheck prop_derivada
--    OK, passed 100 tests.
