-- Numero_de_variables_de_una_expresion_aritmetica.hs
-- Número de variables de una expresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-enero-2023.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas con una variable]
-- (https://bit.ly/40mwjeF), definir la función
--    numVars :: Expr -> Int
-- tal que (numVars e) es el número de variables en la expresión e. Por
-- ejemplo,
--    numVars (C 3)               ==  0
--    numVars X                   ==  1
--    numVars (P X (S (C 13) X))  ==  2
-- ---------------------------------------------------------------------

module Numero_de_variables_de_una_expresion_aritmetica where

import Expresion_aritmetica_con_una_variable (Expr (..))

numVars :: Expr -> Int
numVars X        = 1
numVars (C _)   = 0
numVars (S a b) = numVars a + numVars b
numVars (P a b) = numVars a + numVars b
