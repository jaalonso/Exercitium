-- Pol_Termino_independiente_de_un_polinomio.hs
-- TAD de los polinomios: Término independiente de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los polinomios](https://bit.ly/3KwqXYu)
-- definir la función
--    terminoIndep :: (Num a, Eq a) => Polinomio  a -> a
-- tal que (terminoIndep p) es el término independiente del polinomio
-- p. Por ejemplo,
--    λ> ejPol1 = consPol 4 3 (consPol 2 5 (consPol 0 3 polCero))
--    λ> ejPol1
--    3*x^4 + 5*x^2 + 3
--    λ> terminoIndep ejPol1
--    3
--    λ> ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> ejPol2
--    x^5 + 5*x^2 + 4*x
--    λ> terminoIndep ejPol2
--    0
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Termino_independiente_de_un_polinomio where

import TAD.Polinomio (Polinomio, consPol, polCero)
import Pol_Coeficiente (coeficiente)

terminoIndep :: (Num a, Eq a) => Polinomio  a -> a
terminoIndep = coeficiente 0
