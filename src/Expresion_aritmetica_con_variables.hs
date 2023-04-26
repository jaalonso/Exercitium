-- Expresion_aritmetica_con_variables.hs
-- El tipo de las expresiones aritméticas con variables
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-enero-2023.
-- ---------------------------------------------------------------------

-- La expresión 2*(a+5) puede representarse por
--    P (C 2) (S (V 'a') (C 5))
-- usando el tipo de las expresiones aritméticas con variables definido
-- como se muestra a continuación.

module Expresion_aritmetica_con_variables where

data Expr = C Int
          | V Char
          | S Expr Expr
          | P Expr Expr
  deriving (Eq, Show)
