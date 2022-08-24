-- Numeros_racionales.hs
-- Números racionales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los números racionales pueden representarse mediante pares de números
-- enteros. Por ejemplo, el número 2/5 puede representarse mediante el
-- par (2,5).
--
-- Definir las funciones
--    formaReducida    :: (Int,Int) -> (Int,Int)
--    sumaRacional     :: (Int,Int) -> (Int,Int) -> (Int,Int)
--    productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
--    igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
-- tales que
-- + (formaReducida x) es la forma reducida del número racional x. Por
--   ejemplo,
--      formaReducida (4,10)  ==  (2,5)
--      formaReducida (0,5)   ==  (0,1)
-- + (sumaRacional x y) es la suma de los números racionales x e y,
--   expresada en forma reducida. Por ejemplo,
--      sumaRacional (2,3) (5,6)  ==  (3,2)
--      sumaRacional (3,5) (-3,5) ==  (0,1)
-- + (productoRacional x y) es el producto de los números racionales x e
--   y, expresada en forma reducida. Por ejemplo,
--      productoRacional (2,3) (5,6)  ==  (5,9)
-- + (igualdadRacional x y) se verifica si los números racionales x e y
--   son iguales. Por ejemplo,
--      igualdadRacional (6,9) (10,15)  ==  True
--      igualdadRacional (6,9) (11,15)  ==  False
--      igualdadRacional (0,2) (0,-5)   ==  True
--
-- Comprobar con QuickCheck la propiedad distributiva del producto
-- racional respecto de la suma.
-- ---------------------------------------------------------------------

module Numeros_racionales where

import Test.QuickCheck

formaReducida :: (Int,Int) -> (Int,Int)
formaReducida (0,_) = (0,1)
formaReducida (a,b) = (a `div` c, b  `div` c)
    where c = gcd a b

sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional (a,b) (c,d) = formaReducida (a*d+b*c, b*d)

productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
productoRacional (a,b) (c,d) = formaReducida (a*c, b*d)

igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional (a,b) (c,d) =
    a*d == b*c

-- La propiedad es
prop_distributiva :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Property
prop_distributiva x y z =
  snd x /= 0 && snd y /= 0 && snd z /= 0 ==>
  igualdadRacional (productoRacional x (sumaRacional y z))
                   (sumaRacional (productoRacional x y)
                                 (productoRacional x z))

-- La comprobación es
--    λ> quickCheck prop_distributiva
--    +++ OK, passed 100 tests; 21 discarded.
