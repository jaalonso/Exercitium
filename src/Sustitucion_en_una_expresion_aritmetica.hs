-- Sustitucion_en_una_expresion_aritmetica.hs
-- Sustitución en una expresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas con variables]
-- (https://bit.ly/3HfB0QO), definir la función
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

import Expresion_aritmetica_con_variables (Expr (C, V, S, P))

sustitucion :: Expr -> [(Char, Int)] -> Expr
sustitucion e []          = e
sustitucion (V c) ((d,n):ps)
  | c == d                = C n
  | otherwise             = sustitucion (V c) ps
sustitucion (C n) _      = C n
sustitucion (S e1 e2) ps = S (sustitucion e1 ps) (sustitucion e2 ps)
sustitucion (P e1 e2) ps = P (sustitucion e1 ps) (sustitucion e2 ps)
