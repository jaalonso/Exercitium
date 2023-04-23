-- Pol_Valor_de_un_polinomio_en_un_punto.hs
-- TAD de los polinomios: Valor de un polinomio en un punto.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    valor :: (Num a, Eq a) => Polinomio a -> a -> a
-- tal que (valor p c) es el valor del polinomio p al sustituir su
-- variable por c. Por ejemplo,
--    λ> ejPol = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
--    λ> ejPol
--    3*x^4 + -5*x^2 + 3
--    λ> valor ejPol 0
--    3
--    λ> valor ejPol 1
--    1
--    λ> valor ejPol (-2)
--    31
-- --------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Valor_de_un_polinomio_en_un_punto where

import TAD.Polinomio (Polinomio, polCero, esPolCero, consPol, grado,
                      coefLider, restoPol)

valor :: (Num a, Eq a) => Polinomio a -> a -> a
valor p c
  | esPolCero p = 0
  | otherwise   =  b*c^n + valor r c
  where n = grado p
        b = coefLider p
        r = restoPol p
