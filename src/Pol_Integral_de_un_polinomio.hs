-- Pol_Integral_de_un_polinomio.hs
-- TAD de los polinomios: Integral de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    integral :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
-- tal que (integral p) es la integral del polinomio p cuyos coefientes
-- son números racionales. Por ejemplo,
--    λ> ejPol = consPol 7 2 (consPol 4 5 (consPol 2 5 polCero))
--    λ> ejPol
--    2*x^7 + 5*x^4 + 5*x^2
--    λ> integral ejPol
--    0.25*x^8 + x^5 + 1.6666666666666667*x^3
--    λ> integral ejPol :: Polinomio Rational
--    1 % 4*x^8 + x^5 + 5 % 3*x^3
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Integral_de_un_polinomio where

import TAD.Polinomio (Polinomio, polCero, consPol, esPolCero, grado,
                      coefLider, restoPol)
import Data.Ratio

integral :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
integral p
  | esPolCero p = polCero
  | otherwise   = consPol (n+1) (b / fromIntegral (n+1)) (integral r)
  where n = grado p
        b = coefLider p
        r = restoPol p
