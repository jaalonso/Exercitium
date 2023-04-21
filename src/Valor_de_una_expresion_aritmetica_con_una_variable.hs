-- Valor_de_una_expresion_aritmetica_con_una_variable.hs
-- Valor de una expresión aritmética con una variable.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas con una variable](https://bit.ly/40mwjeF),
-- definir la función
--    valor :: Expr -> Int -> Int
-- tal que (valor e n) es el valor de la expresión e cuando se
-- sustituye su variable por n. Por ejemplo,
--    valor (P X (S (C 13) X)) 2  ==  30
-- ---------------------------------------------------------------------

module Valor_de_una_expresion_aritmetica_con_una_variable where

import Expresion_aritmetica_con_una_variable (Expr (..))

valor :: Expr -> Int -> Int
valor X         n = n
valor (C a)     _ = a
valor (S e1 e2) n = valor e1 n + valor e2 n
valor (P e1 e2) n = valor e1 n * valor e2 n
