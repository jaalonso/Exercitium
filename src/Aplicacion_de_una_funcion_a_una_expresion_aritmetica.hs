-- Aplicacion_de_una_funcion_a_una_expresion_aritmetica.hs
-- Aplicación de una función a una expresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las expresiones aritméticas básicas pueden representarse usando el
-- siguiente tipo de datos
--    data Expr = C Int
--              | S Expr Expr
--              | P Expr Expr
--      deriving (Show, Eq)
-- Por ejemplo, la expresión 2*(3+7) se representa por
--    P (C 2) (S (C 3) (C 7))
--
-- Definir la función
--    aplica :: (Int -> Int) -> Expr -> Expr
-- tal que (aplica f e) es la expresión obtenida aplicando la función f
-- a cada uno de los números de la expresión e. Por ejemplo,
--    λ> aplica (+2) (S (P (C 3) (C 5)) (P (C 6) (C 7)))
--    S (P (C 5) (C 7)) (P (C 8) (C 9))
--    λ> aplica (*2) (S (P (C 3) (C 5)) (P (C 6) (C 7)))
--    S (P (C 6) (C 10)) (P (C 12) (C 14))
-- ---------------------------------------------------------------------

module Aplicacion_de_una_funcion_a_una_expresion_aritmetica where

data Expr = C Int
          | S Expr Expr
          | P Expr Expr
  deriving (Show, Eq)

aplica :: (Int -> Int) -> Expr -> Expr
aplica f (C x)     = C (f x)
aplica f (S e1 e2) = S (aplica f e1) (aplica f e2)
aplica f (P e1 e2) = P (aplica f e1) (aplica f e2)
