-- Sustitucion_en_una_expresion_aritmetica.hs
-- Sustitución en una expresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las expresiones aritméticas con variables pueden representarse usando
-- el siguiente tipo de datos
--    data Expr = C Int
--              | V Char
--              | S Expr Expr
--              | P Expr Expr
--      deriving (Eq, Show)
-- Por ejemplo, la expresión 2*(a+5) se representa por
--    P (C 2) (S (V 'a') (C 5))
--
-- Definir la función
--    sustitucion :: Expr -> [(Char, Int)] -> Expr
-- tal que (sustitucion e s) es la expresión obtenida sustituyendo las
-- variables de la expresión e según se indica en la sustitución s. Por
-- ejemplo,
--    λ> sustitucion (P (V 'z') (S (C 3) (V 'x'))) [('x',7),('z',9)]
--    P (C 9) (S (C 3) (C 7))
--    λ> sustitucion (P (V 'z') (S (C 3) (V 'y'))) [('x',7),('z',9)]
--    P (C 9) (S (C 3) (V 'y'))
-- ---------------------------------------------------------------------

module Sustitucion_en_una_expresion_aritmetica where

data Expr = C Int
          | V Char
          | S Expr Expr
          | P Expr Expr
  deriving (Eq, Show)

sustitucion :: Expr -> [(Char, Int)] -> Expr
sustitucion e []          = e
sustitucion (V c) ((d,n):ps)
  | c == d                = C n
  | otherwise             = sustitucion (V c) ps
sustitucion (C n) _      = C n
sustitucion (S e1 e2) ps = S (sustitucion e1 ps) (sustitucion e2 ps)
sustitucion (P e1 e2) ps = P (sustitucion e1 ps) (sustitucion e2 ps)
