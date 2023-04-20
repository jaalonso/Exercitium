-- Valor_de_una_expresion_aritmetica.hs
-- El tipo de las expresiones aritméticas: Valor_de_una_expresión.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas](https://bit.ly/40vCQUh),
-- definir la función
--    valor :: Expr -> Int
-- tal que (valor e) es el valor de la expresión e (donde el valor de
-- (SiCero e e1 e2) es el valor de e1 si el valor de e es cero y el es
-- el valor de e2, en caso contrario). Por ejemplo,
--    valor (Op (Suma (Lit 3) (Lit 5)))      ==  -8
--    valor (SiCero (Lit 0) (Lit 4) (Lit 5)) == 4
--    valor (SiCero (Lit 1) (Lit 4) (Lit 5)) == 5
-- ---------------------------------------------------------------------

module Valor_de_una_expresion_aritmetica where

import Tipo_expresion_aritmetica (Expr (..))

valor :: Expr -> Int
valor (Lit n)        = n
valor (Suma x y)     = valor x + valor y
valor (Op x)         = - valor x
valor (SiCero x y z) | valor x == 0 = valor y
                     | otherwise    = valor z
