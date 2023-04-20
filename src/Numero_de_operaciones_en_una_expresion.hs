-- Numero_de_operaciones_en_una_expresion.hs
-- El tipo de las expresiones aritméticas: Número de operaciones en una expresión.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas](https://bit.ly/40vCQUh),
-- definir la función
--    numeroOps :: Expr -> Int
-- tal que (numeroOps e) es el número de operaciones de e. Por ejemplo,
--    numeroOps (Lit 3)                      ==  0
--    numeroOps (Suma (Lit 7) (Op (Lit 5)))  ==  2
-- ---------------------------------------------------------------------

module Numero_de_operaciones_en_una_expresion where

import Tipo_expresion_aritmetica (Expr (..))

numeroOps :: Expr -> Int
numeroOps (Lit _)        = 0
numeroOps (Suma x y)     = 1 + numeroOps x + numeroOps y
numeroOps (Op x)         = 1 + numeroOps x
numeroOps (SiCero x y z) = 1 + numeroOps x + numeroOps y + numeroOps z
