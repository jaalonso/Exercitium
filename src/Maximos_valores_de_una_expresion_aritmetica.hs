-- Maximos_valores_de_una_expresion_aritmetica.hs
-- Máximos valores de una expresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las expresiones aritméticas generales se pueden definir usando el
-- siguiente tipo de datos
--    data Expr = C Int
--               | X
--               | S Expr Expr
--               | R Expr Expr
--               | P Expr Expr
--               | E Expr Int
--      deriving (Eq, Show)
-- Por ejemplo, la expresión
--    3*x - (x+2)^7
-- se puede definir por
--    R (P (C 3) X) (E (S X (C 2)) 7)
--
-- Definir la función
--    maximo :: Expr -> [Int] -> (Int,[Int])
-- tal que (maximo e xs) es el par formado por el máximo valor de la
-- expresión e para los puntos de xs y en qué puntos alcanza el
-- máximo. Por ejemplo,
--    λ> maximo (E (S (C 10) (P (R (C 1) X) X)) 2) [-3..3]
--    (100,[0,1])
-- ---------------------------------------------------------------------

module Maximos_valores_de_una_expresion_aritmetica where

data Expr = C Int
           | X
           | S Expr Expr
           | R Expr Expr
           | P Expr Expr
           | E Expr Int
  deriving (Eq, Show)

maximo :: Expr -> [Int] -> (Int,[Int])
maximo e ns = (m,[n | n <- ns, valor e n == m])
  where m = maximum [valor e n | n <- ns]

valor :: Expr -> Int -> Int
valor (C x) _     = x
valor X     n     = n
valor (S e1 e2) n = valor e1 n + valor e2 n
valor (R e1 e2) n = valor e1 n - valor e2 n
valor (P e1 e2) n = valor e1 n * valor e2 n
valor (E e1 m1) n = valor e1 n ^ m1
