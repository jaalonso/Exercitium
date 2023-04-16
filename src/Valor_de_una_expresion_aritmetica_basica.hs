-- Valor_de_una_expresion_aritmetica_basica.hs
-- Valor de una expresión aritmética básica.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas básicas]
-- (https://bit.ly/43EuWL4), definir la función
--    valor :: Expr -> Int
-- tal que (valor e) es el valor de la expresión aritmética e. Por
-- ejemplo,
--    valor (P (C 2) (S (C 3) (C 7)))  ==  20
-- ---------------------------------------------------------------------

module Valor_de_una_expresion_aritmetica_basica where

import Expresion_aritmetica_basica (Expr(..))

valor :: Expr -> Int
valor (C x)   = x
valor (S x y) = valor x + valor y
valor (P x y) = valor x * valor y
