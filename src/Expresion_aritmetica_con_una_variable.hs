-- Expresion_aritmetica_con_una_variable.hs
-- Tipo de expresiones aritméticas con una variable.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-enero-2023
-- ---------------------------------------------------------------------

-- La expresión X*(13+X) se representa por
--    P(X(), S(C(13), X()))
-- usando el tipo de las expresiones aritméticas con una variable
-- (denotada por X) que se define como se muestra a continuación,

module Expresion_aritmetica_con_una_variable where

data Expr = X
          | C Int
          | S Expr Expr
          | P Expr Expr
