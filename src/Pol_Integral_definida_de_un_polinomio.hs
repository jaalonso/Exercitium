-- Pol_Integral_definida_de_un_polinomio.hs
-- TAD de los polinomios: Integral definida de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los polinomios](https://bit.ly/3KwqXYu)
-- definir la función
--    integralDef :: (Fractional t, Eq t) => Polinomio t -> t -> t -> t
-- tal que (integralDef p a b) es la integral definida del polinomio p
-- entre a y b. Por ejemplo,
--    λ> ejPol = consPol 7 2 (consPol 4 5 (consPol 2 5 polCero))
--    λ> ejPol
--    2*x^7 + 5*x^4 + 5*x^2
--    λ> integralDef ejPol 0 1
--    2.916666666666667
--    λ> integralDef ejPol 0 1 :: Rational
--    35 % 12
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Integral_definida_de_un_polinomio where

import TAD.Polinomio (Polinomio, consPol, polCero)
import Pol_Valor_de_un_polinomio_en_un_punto (valor)
import Pol_Integral_de_un_polinomio (integral)

integralDef :: (Fractional t, Eq t) => Polinomio t -> t -> t -> t
integralDef p a b = valor q b - valor q a
  where q = integral p
