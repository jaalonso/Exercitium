-- Pol_Multiplicacion_de_un_polinomio_por_un_numero.hs
-- TAD de los polinomios: Multiplicación de un polinomio por un número.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    multEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
-- tal que (multEscalar c p) es el polinomio obtenido multiplicando el
-- número c por el polinomio p. Por ejemplo,
--    λ> ejPol = consPol 1 2 (consPol 0 3 polCero)
--    λ> ejPol
--    2*x + 3
--    λ> multEscalar 4 ejPol
--    8*x + 12
--    λ> multEscalar (1 % 4) ejPol
--    1 % 2*x + 3 % 4
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Multiplicacion_de_un_polinomio_por_un_numero where

import TAD.Polinomio (Polinomio, polCero, esPolCero, consPol, grado,
                      coefLider, restoPol)
import Data.Ratio

multEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
multEscalar c p
  | esPolCero p = polCero
  | otherwise   = consPol n (c*b) (multEscalar c r)
  where n = grado p
        b = coefLider p
        r = restoPol p
