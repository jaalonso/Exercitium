-- Numero_de_sumas_en_una_expresion_aritmetica.hs
-- Número de sumas en una expresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas con variables]
-- (https://bit.ly/3HfB0QO), definir la función
--    sumas :: Expr -> Int
-- tal que (sumas e) es el número de sumas en la expresión e. Por
-- ejemplo,
--    sumas (P (V 'z') (S (C 3) (V 'x')))  ==  1
--    sumas (S (V 'z') (S (C 3) (V 'x')))  ==  2
--    sumas (P (V 'z') (P (C 3) (V 'x')))  ==  0
-- ---------------------------------------------------------------------

module Numero_de_sumas_en_una_expresion_aritmetica where

import Expresion_aritmetica_con_variables (Expr (C, V, S, P))

sumas :: Expr -> Int
sumas (V _)   = 0
sumas (C _)   = 0
sumas (S x y) = 1 + sumas x + sumas y
sumas (P x y) = sumas x + sumas y
