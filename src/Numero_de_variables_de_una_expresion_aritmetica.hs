-- Numero_de_variables_de_una_expresion_aritmetica.hs
-- Número de variables de una expresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-enero-2023.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las expresiones aritméticas construidas con una variable (denotada
-- por X), los números enteros y las operaciones de sumar y multiplicar
-- se pueden representar mediante el tipo de datos Expr definido por
--    data Expr = X
--              | C Int
--              | S Expr Expr
--              | P Expr Expr
-- Por ejemplo, la expresión X*(13+X) se representa por
--    P X (S (C 13) X)
--
-- Definir la función
--    numVars :: Expr -> Int
-- tal que (numVars e) es el número de variables en la expresión e. Por
-- ejemplo,
--    numVars (C 3)               ==  0
--    numVars X                   ==  1
--    numVars (P X (S (C 13) X))  ==  2
-- ---------------------------------------------------------------------

module Numero_de_variables_de_una_expresion_aritmetica where

data Expr = X
          | C Int
          | S Expr Expr
          | P Expr Expr

numVars :: Expr -> Int
numVars X        = 1
numVars (C _)   = 0
numVars (S a b) = numVars a + numVars b
numVars (P a b) = numVars a + numVars b
