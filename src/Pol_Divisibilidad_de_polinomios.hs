-- Pol_Divisibilidad_de_polinomios.hs
-- TAD de los polinomios: Divisibilidad de polinomios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    divisiblePol :: (Fractional a, Eq a) =>
--                    Polinomio a -> Polinomio a -> Bool
-- tal que (divisiblePol p q) se verifica si el polinomio p es divisible
-- por el polinomio q. Por ejemplo,
--    λ> pol1 = consPol 2 8 (consPol 1 14 (consPol 0 3 polCero))
--    λ> pol1
--    8*x^2 + 14*x + 3
--    λ> pol2 = consPol 1 2 (consPol 0 3 polCero)
--    λ> pol2
--    2*x + 3
--    λ> pol3 = consPol 2 6 (consPol 1 2 polCero)
--    λ> pol3
--    6*x^2 + 2*x
--    λ> divisiblePol pol1 pol2
--    True
--    λ> divisiblePol pol1 pol3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Divisibilidad_de_polinomios where

import TAD.Polinomio (Polinomio, polCero, consPol, esPolCero)
import Pol_Division_de_polinomios (resto)

divisiblePol :: (Fractional a, Eq a) =>
                Polinomio a -> Polinomio a -> Bool
divisiblePol p q = esPolCero (resto p q)
