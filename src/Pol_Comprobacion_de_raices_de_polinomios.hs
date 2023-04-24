-- Pol_Comprobacion_de_raices_de_polinomios.hs
-- TAD de los polinomios: Comprobación de raíces de polinomios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
-- (esRaiz c p) se verifica si c es una raiz del polinomio p. por
-- ejemplo,
--    λ> ejPol = consPol 4 6 (consPol 1 2 polCero)
--    λ> ejPol
--    6*x^4 + 2*x
--    λ> esRaiz 0 ejPol
--    True
--    λ> esRaiz 1 ejPol
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Comprobacion_de_raices_de_polinomios where

import TAD.Polinomio (Polinomio, polCero, consPol)
import Pol_Valor_de_un_polinomio_en_un_punto (valor)

esRaiz :: (Num a, Eq a) => a -> Polinomio a -> Bool
esRaiz c p = valor p c == 0
