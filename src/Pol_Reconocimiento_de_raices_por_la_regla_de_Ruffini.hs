-- Pol_Reconocimiento_de_raices_por_la_regla_de_Ruffini.hs
-- TAD de los polinomios: Reconocimiento de raíces por la regla de Ruffini.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    esRaizRuffini:: Int -> Polinomio Int -> Bool
-- tal que (esRaizRuffini r p) se verifica si r es una raiz de p, usando
-- para ello el regla de Ruffini. Por ejemplo,
--    λ> ejPol = consPol 4 6 (consPol 1 2 polCero)
--    λ> ejPol
--    6*x^4 + 2*x
--    λ> esRaizRuffini 0 ejPol
--    True
--    λ> esRaizRuffini 1 ejPol
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Reconocimiento_de_raices_por_la_regla_de_Ruffini where

import TAD.Polinomio (Polinomio, consPol, polCero)
import Pol_Regla_de_Ruffini (restoRuffini)

esRaizRuffini :: Int -> Polinomio Int -> Bool
esRaizRuffini r p = restoRuffini r p == 0
