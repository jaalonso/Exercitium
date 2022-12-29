-- Valor_de_una_expresion_aritmetica_con_una_variable.hs
-- Valor de una expresión aritmética con una variable.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-enero-2023
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
--    valor :: Expr -> Int -> Int
-- tal que (valor e n) es el valor de la expresión e cuando se
-- sustituye su variable por n. Por ejemplo,
--    valor (P X (S (C 13) X)) 2  ==  30
-- ---------------------------------------------------------------------

module Valor_de_una_expresion_aritmetica_con_una_variable where

data Expr = X
          | C Int
          | S Expr Expr
          | P Expr Expr

valor :: Expr -> Int -> Int
valor X         n = n
valor (C a)     _ = a
valor (S e1 e2) n = valor e1 n + valor e2 n
valor (P e1 e2) n = valor e1 n * valor e2 n
