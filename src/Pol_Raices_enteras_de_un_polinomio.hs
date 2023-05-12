-- Pol_Raices_enteras_de_un_polinomio.hs
-- TAD de los polinomios: Raíces enteras de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--     raicesRuffini :: Polinomio Int -> [Int]
-- tal que (raicesRuffini p) es la lista de las raices enteras de p,
-- calculadas usando el regla de Ruffini. Por ejemplo,
--     λ> ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
--     λ> ejPol1
--     3*x^4 + -5*x^2 + 3
--     λ> raicesRuffini ejPol1
--     []
--     λ> ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--     λ> ejPol2
--     x^5 + 5*x^2 + 4*x
--     λ> raicesRuffini ejPol2
--     [0,-1]
--     λ> ejPol3 = consPol 4 6 (consPol 1 2 polCero)
--     λ> ejPol3
--     6*x^4 + 2*x
--     λ> raicesRuffini ejPol3
--     [0]
--     λ> ejPol4 = consPol 3 1 (consPol 2 2 (consPol 1 (-1) (consPol 0 (-2) polCero)))
--     λ> ejPol4
--     x^3 + 2*x^2 + -1*x + -2
--     λ> raicesRuffini ejPol4
--     [1,-1,-2]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Raices_enteras_de_un_polinomio where

import TAD.Polinomio (Polinomio, consPol, polCero, esPolCero)
import Pol_Termino_independiente_de_un_polinomio (terminoIndep)
import Pol_Regla_de_Ruffini (cocienteRuffini)
import Pol_Reconocimiento_de_raices_por_la_regla_de_Ruffini (esRaizRuffini)

raicesRuffini :: Polinomio Int -> [Int]
raicesRuffini p
  | esPolCero p = []
  | otherwise   = aux (0 : divisores (terminoIndep p))
  where aux [] = []
        aux (r:rs)
          | esRaizRuffini r p = r : raicesRuffini (cocienteRuffini r p)
          | otherwise         = aux rs

-- (divisores n) es la lista de todos los divisores enteros de n. Por
-- ejemplo,
--    divisores 4     ==  [1,-1,2,-2,4,-4]
--    divisores (-6)  ==  [1,-1,2,-2,3,-3,6,-6]
divisores :: Int -> [Int]
divisores n = concat [[x,-x] | x <- [1..abs n], rem n x == 0]
