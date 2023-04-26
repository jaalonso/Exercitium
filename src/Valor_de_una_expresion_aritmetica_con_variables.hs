-- Valor_de_una_expresion_aritmetica_con_variables.hs
-- Valor de una expresión aritmética con variables.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-enero-2023.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas con variables]
-- (https://bit.ly/3HfB0QO), definir la función
--    valor :: Expr -> [(Char,Int)] -> Int
-- tal que (valor x e) es el valor de la expresión x en el entorno e (es
-- decir, el valor de la expresión donde las variables de x se sustituyen
-- por los valores según se indican en el entorno e). Por ejemplo,
--    λ> valor (P (C 2) (S (V 'a') (V 'b'))) [('a',2),('b',5)]
--    14
-- ---------------------------------------------------------------------

module Valor_de_una_expresion_aritmetica_con_variables where

import Expresion_aritmetica_con_variables (Expr (C, V, S, P))

valor :: Expr -> [(Char,Int)] -> Int
valor (C x)   _ = x
valor (V x)   e = head [y | (z,y) <- e, z == x]
valor (S x y) e = valor x e + valor y e
valor (P x y) e = valor x e * valor y e
