-- Valor_de_expresiones_aritmeticas_generales.hs
-- Valor de expresiones aritméticas generales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las operaciones de suma, resta y  multiplicación se pueden
-- representar mediante el siguiente tipo de datos
--    data Op = S | R | M
-- La expresiones aritméticas con dichas operaciones se pueden
-- representar mediante el siguiente tipo de dato algebraico
--    data Expr = C Int
--              | A Op Expr Expr
-- Por ejemplo, la expresión
--    (7-3)+(2*5)
-- se representa por
--    A S (A R (C 7) (C 3)) (A M (C 2) (C 5))
--
-- Definir la función
--    valor :: Expr -> Int
-- tal que (valor e) es el valor de la expresión e. Por ejemplo,
--    valor (A S (A R (C 7) (C 3)) (A M (C 2) (C 5)))  ==  14
--    valor (A M (A R (C 7) (C 3)) (A S (C 2) (C 5)))  ==  28
-- ---------------------------------------------------------------------

module Valor_de_expresiones_aritmeticas_generales where

data Op = S | R | M

data Expr = C Int
          | A Op Expr Expr

-- 1ª solución
-- ===========

valor :: Expr -> Int
valor (C x)      = x
valor (A o e1 e2) = aplica o (valor e1) (valor e2)
  where aplica :: Op -> Int -> Int -> Int
        aplica S x y = x+y
        aplica R x y = x-y
        aplica M x y = x*y

-- 2ª solución
-- ===========

valor2 :: Expr -> Int
valor2 (C n)    = n
valor2 (A o x y) = sig o (valor2 x) (valor2 y)
  where sig :: Op -> Int -> Int -> Int
        sig S = (+)
        sig M = (*)
        sig R = (-)
