-- Arbol_de_factorizacion.hs
-- Árbol de factorización.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los divisores medios de un número son los que ocupan la posición
-- media entre los divisores de n, ordenados de menor a mayor. Por
-- ejemplo, los divisores de 60 son [1,2,3,4,5,6,10,12,15,20,30,60] y
-- sus divisores medios son 6 y 10. Para los números que son cuadrados
-- perfectos, sus divisores medios de son sus raíces cuadradas; por
-- ejemplos, los divisores medios de 9 son 3 y 3.
--
-- El árbol de factorización de un número compuesto n se construye de la
-- siguiente manera:
--    * la raíz es el número n,
--    * la rama izquierda es el árbol de factorización de su divisor
--      medio menor y
--    * la rama derecha es el árbol de factorización de su divisor
--      medio mayor
-- Si el número es primo, su árbol de factorización sólo tiene una hoja
-- con dicho número. Por ejemplo, el árbol de factorización de 60 es
--        60
--       /  \
--      6    10
--     / \   / \
--    2   3 2   5
--
-- Definir la función
--    arbolFactorizacion :: Int -> Arbol
-- tal que (arbolFactorizacion n) es el árbol de factorización de n. Por
-- ejemplo,
--    arbolFactorizacion 60 == N 60 (N 6 (H 2) (H 3)) (N 10 (H 2) (H 5))
--    arbolFactorizacion 45 == N 45 (H 5) (N 9 (H 3) (H 3))
--    arbolFactorizacion 7  == H 7
--    arbolFactorizacion 9  == N 9 (H 3) (H 3)
--    arbolFactorizacion 14 == N 14 (H 2) (H 7)
--    arbolFactorizacion 28 == N 28 (N 4 (H 2) (H 2)) (H 7)
--    arbolFactorizacion 84 == N 84 (H 7) (N 12 (H 3) (N 4 (H 2) (H 2)))
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Arbol_de_factorizacion where

import Test.QuickCheck

-- 1ª solución
-- ===========

data Arbol = H Int
           | N Int Arbol Arbol
  deriving (Eq, Show)

arbolFactorizacion1 :: Int -> Arbol
arbolFactorizacion1 n
  | esPrimo n = H n
  | otherwise = N n (arbolFactorizacion1 x) (arbolFactorizacion1 y)
  where (x,y) = divisoresMedio n

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 9  ==  False
esPrimo :: Int -> Bool
esPrimo n = divisores n == [1,n]

-- (divisoresMedio n) es el par formado por los divisores medios de
-- n. Por ejemplo,
--    divisoresMedio 30  ==  (5,6)
--    divisoresMedio  7  ==  (1,7)
--    divisoresMedio 16  ==  (4,4)
divisoresMedio :: Int -> (Int,Int)
divisoresMedio n = (n `div` x,x)
  where xs = divisores n
        x  = xs !! (length xs `div` 2)

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `rem` x == 0]

-- 2ª solución
-- ===========

arbolFactorizacion2 :: Int -> Arbol
arbolFactorizacion2 n
  | x == 1    = H n
  | otherwise = N n (arbolFactorizacion2 x) (arbolFactorizacion2 y)
  where (x,y) = divisoresMedio n

-- (divisoresMedio2 n) es el par formado por los divisores medios de
-- n. Por ejemplo,
--    divisoresMedio2 30  ==  (5,6)
--    divisoresMedio2  7  ==  (1,7)
divisoresMedio2 :: Int -> (Int,Int)
divisoresMedio2 n = (n `div` x,x)
  where m = ceiling (sqrt (fromIntegral n))
        x = head [y | y <- [m..n], n `rem` y == 0]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_arbolFactorizacion :: Int -> Property
prop_arbolFactorizacion n =
  n > 1 ==> arbolFactorizacion1 n == arbolFactorizacion2 n

-- La comprobación es
--    λ> quickCheck prop_arbolFactorizacion
--    +++ OK, passed 100 tests; 162 discarded.
