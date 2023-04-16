-- Expresion_aritmetica_basica.hs
-- Expresión aritmética básica.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-enero-2023
-- ---------------------------------------------------------------------

-- La expresión aritmética 2*(3+7) se representa por
--    P (C 2) (S (C 3) (C 7))
-- usando el tipo de dato definido a continuación.

module Expresion_aritmetica_basica where

data Expr = C Int
          | S Expr Expr
          | P Expr Expr
  deriving (Eq, Show)
