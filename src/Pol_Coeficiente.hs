-- Pol_Coeficiente.hs
-- TAD de los polinomios: Coeficiente del término de grado k.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los polinomios](https://bit.ly/3KwqXYu)
-- definir la función
--    coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
-- tal que (coeficiente k p) es el coeficiente del término de grado k
-- del polinomio p. Por ejemplo,
--    λ> ejPol = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> ejPol
--    x^5 + 5*x^2 + 4*x
--    λ> coeficiente 2 ejPol
--    5
--    λ> coeficiente 3 ejPol
--    0
-- ------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Coeficiente where

import TAD.Polinomio (Polinomio, coefLider, grado, restoPol,
                      consPol, polCero)

coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente k p | k == n                 = coefLider p
                | k > grado (restoPol p) = 0
                | otherwise              = coeficiente k (restoPol p)
  where n = grado p
