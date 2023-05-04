-- Pol_Metodo_de_Horner_del_valor_de_un_polinomio.hs
-- TAD de los polinomios: Método de Horner del valor de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El método de Horner para calcular el valor de un  polinomio se basa
-- en representarlo de una forma forma alernativa. Por ejemplo, para
-- calcular el valor de
--    a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f
-- se representa como
--   (((((0 * x + a) * x + b) * x + c) * x + d) * x + e) * x + f
-- y se evalúa de dentro hacia afuera; es decir,
--   v(0) = 0
--   v(1) = v(0)*x+a = 0*x+a = a
--   v(2) = v(1)*x+b = a*x+b
--   v(3) = v(2)*x+c = (a*x+b)*x+c = a*x^2+b*x+c
--   v(4) = v(3)*x+d = (a*x^2+b*x+c)*x+d = a*x^3+b*x^2+c*x+d
--   v(5) = v(4)*x+e = (a*x^3+b*x^2+c*x+d)*x+e = a*x^4+b*x^3+c*x^2+d*x+e
--   v(6) = v(5)*x+f = (a*x^4+b*x^3+c*x^2+d*x+e)*x+f = a*x^5+b*x^4+c*x^3+d*x^2+e*x+f
--
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    horner :: (Num a, Eq a) => Polinomio a -> a -> a
-- tal que (horner p x) es el valor del polinomio p al sustituir su
-- variable por el número x. Por ejemplo,
--    λ> pol1 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> pol1
--    x^5 + 5*x^2 + 4*x
--    λ> horner pol1 0
--    0
--    λ> horner pol1 1
--    10
--    λ> horner pol1 1.5
--    24.84375
--    λ> import Data.Ratio
--    λ> horner pol1 (3%2)
--    795 % 32
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Metodo_de_Horner_del_valor_de_un_polinomio where

import TAD.Polinomio (Polinomio, polCero, consPol)
import Pol_Transformaciones_polinomios_densas (polinomioAdensa)

-- 1ª solución
-- ===========

horner :: (Num a, Eq a) => Polinomio a -> a -> a
horner p x = hornerAux (polinomioAdensa p) 0
  where hornerAux [] v     = v
        hornerAux (a:as) v = hornerAux as (v*x+a)

-- El cálculo de (horner pol1 2) es el siguiente
--    horner pol1 2
--    = hornerAux [1,0,0,5,4,0] 0
--    = hornerAux   [0,0,5,4,0] ( 0*2+1) = hornerAux   [0,0,5,4,0] 1
--    = hornerAux     [0,5,4,0] ( 1*2+0) = hornerAux     [0,5,4,0] 2
--    = hornerAux       [5,4,0] ( 2*2+0) = hornerAux       [5,4,0] 4
--    = hornerAux         [4,0] ( 4*2+5) = hornerAux         [4,0] 13
--    = hornerAux           [0] (13*2+4) = hornerAux           [0] 30
--    = hornerAux            [] (30*2+0) = hornerAux            [] 60

-- 2ª solución
-- ===========

horner2 :: (Num a, Eq a) => Polinomio a -> a -> a
horner2 p x = foldl (\a b -> a*x + b) 0 (polinomioAdensa p)
