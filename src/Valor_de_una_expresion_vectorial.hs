-- Valor_de_una_expresion_vectorial.hs
-- Valor de una expresión vectorial.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se consideran las expresiones vectoriales formadas por un vector, la
-- suma de dos expresiones vectoriales o el producto de un entero por
-- una expresión vectorial. El siguiente tipo de dato define las
-- expresiones vectoriales
--    data ExpV = Vec Int Int
--              | Sum ExpV ExpV
--              | Mul Int ExpV
--      deriving Show
--
-- Definir la función
--    valorEV :: ExpV -> (Int,Int)
-- tal que (valorEV e) es el valorEV de la expresión vectorial c. Por
-- ejemplo,
--    valorEV (Vec 1 2)                                  ==  (1,2)
--    valorEV (Sum (Vec 1 2) (Vec 3 4))                  ==  (4,6)
--    valorEV (Mul 2 (Vec 3 4))                          ==  (6,8)
--    valorEV (Mul 2 (Sum (Vec 1 2 ) (Vec 3 4)))         ==  (8,12)
--    valorEV (Sum (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))  ==  (8,12)
-- ---------------------------------------------------------------------

module Valor_de_una_expresion_vectorial where

data ExpV = Vec Int Int
          | Sum ExpV ExpV
          | Mul Int ExpV
  deriving Show

-- 1ª solución
-- ===========

valorEV1 :: ExpV -> (Int,Int)
valorEV1 (Vec x y)   = (x,y)
valorEV1 (Sum e1 e2) = (x1+x2,y1+y2)
  where (x1,y1) = valorEV1 e1
        (x2,y2) = valorEV1 e2
valorEV1 (Mul n e)   = (n*x,n*y)
  where (x,y) = valorEV1 e

-- 2ª solución
-- ===========

valorEV2 :: ExpV -> (Int,Int)
valorEV2 (Vec a b)   = (a, b)
valorEV2 (Sum e1 e2) = suma (valorEV2 e1) (valorEV2 e2)
valorEV2 (Mul n e1)  = multiplica n (valorEV2 e1)

suma :: (Int,Int) -> (Int,Int) -> (Int,Int)
suma (a,b) (c,d) = (a+c,b+d)

multiplica :: Int -> (Int, Int) -> (Int, Int)
multiplica n (a,b) = (n*a,n*b)
