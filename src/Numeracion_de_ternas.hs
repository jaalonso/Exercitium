-- Numeracion_de_ternas.hs
-- Numeración de las ternas de números naturales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-Mayo-2014 (actualizado 28-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las ternas de números naturales se pueden ordenar como sigue
--    (0,0,0),
--    (0,0,1),(0,1,0),(1,0,0),
--    (0,0,2),(0,1,1),(0,2,0),(1,0,1),(1,1,0),(2,0,0),
--    (0,0,3),(0,1,2),(0,2,1),(0,3,0),(1,0,2),(1,1,1),(1,2,0),(2,0,1),...
--    ...
--
-- Definir la función
--    posicion :: (Int,Int,Int) -> Int
-- tal que (posicion (x,y,z)) es la posición de la terna de números
-- naturales (x,y,z) en la ordenación anterior. Por ejemplo,
--    posicion (0,1,0)  ==  2
--    posicion (0,0,2)  ==  4
--    posicion (0,1,1)  ==  5
--
-- Comprobar con QuickCheck que
-- + la posición de (x,0,0) es x(x²+6x+11)/6
-- + la posición de (0,y,0) es y(y²+3y+ 8)/6
-- + la posición de (0,0,z) es z(z²+3z+ 2)/6
-- + la posición de (x,x,x) es x(9x²+14x+7)/2
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Numeracion_de_ternas where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

posicion1 :: (Int,Int,Int) -> Int
posicion1 t = aux 0 ternas
  where aux n (t':ts) | t' == t   = n
                      | otherwise = aux (n+1) ts

-- ternas es la lista ordenada de las ternas de números naturales. Por ejemplo,
--    λ> take 9 ternas
--    [(0,0,0),(0,0,1),(0,1,0),(1,0,0),(0,0,2),(0,1,1),(0,2,0),(1,0,1),(1,1,0)]
ternas :: [(Int,Int,Int)]
ternas = [(x,y,n-x-y) | n <- [0..], x <- [0..n], y <- [0..n-x]]

-- 2ª solución
-- ===========

posicion2 :: (Int,Int,Int) -> Int
posicion2 t =
  head [n | (n,t') <- zip [0..] ternas, t' == t]

-- 3ª solución
-- ===========

posicion3 :: (Int,Int,Int) -> Int
posicion3 t = indice t ternas

-- (indice x ys) es el índice de x en ys. Por ejemplo,
--    indice 5 [0..]  ==  5
indice :: Eq a => a -> [a] -> Int
indice x ys = length (takeWhile (/= x) ys)

-- 4ª solución
-- ===========

posicion4 :: (Int,Int,Int) -> Int
posicion4 t = fromJust (elemIndex t ternas)

-- 5ª solución
-- ===========

posicion5 :: (Int,Int,Int) -> Int
posicion5 = fromJust . (`elemIndex` ternas)

-- 6ª solución
-- ===========

posicion6 :: (Int,Int,Int) -> Int
posicion6 (x,y,z) =
  sum [((n+2)*(n+1)) `div` 2 | n <- [0..s-1]] +
  sum [s-i+1 | i <- [0..x-1]] + y
  where s = x + y + z

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Int,Int,Int) -> Int) -> Spec
specG posicion = do
  it "e1" $
    posicion (0,1,0)  `shouldBe`  2
  it "e2" $
    posicion (0,0,2)  `shouldBe`  4
  it "e3" $
    posicion (0,1,1)  `shouldBe`  5

spec :: Spec
spec = do
  describe "def. 1" $ specG posicion1
  describe "def. 2" $ specG posicion2
  describe "def. 3" $ specG posicion3
  describe "def. 4" $ specG posicion4
  describe "def. 5" $ specG posicion5
  describe "def. 6" $ specG posicion6

-- La verificación es
--    λ> verifica
--    15 examples, 0 failures

-- Equivalencia
-- ============

-- La propiedad es
prop_posicion_equiv :: NonNegative Int
                    -> NonNegative Int
                    -> NonNegative Int
                    -> Bool
prop_posicion_equiv (NonNegative x) (NonNegative y) (NonNegative z) =
  all (== posicion1 (x,y,z))
      [f (x,y,z) | f <- [ posicion2
                        , posicion3
                        , posicion4
                        , posicion5
                        , posicion6 ]]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> posicion1 (147,46,116)
--    5000000
--    (5.84 secs, 2,621,428,184 bytes)
--    λ> posicion2 (147,46,116)
--    5000000
--    (3.63 secs, 2,173,230,200 bytes)
--    λ> posicion3 (147,46,116)
--    5000000
--    (2.48 secs, 1,453,229,880 bytes)
--    λ> posicion4 (147,46,116)
--    5000000
--    (1.91 secs, 1,173,229,840 bytes)
--    λ> posicion5 (147,46,116)
--    5000000
--    (1.94 secs, 1,173,229,960 bytes)
--    λ> posicion6 (147,46,116)
--    5000000
--    (0.01 secs, 779,568 bytes)

-- Propiedades
-- ===========

-- La 1ª propiedad es
prop_posicion1 :: NonNegative Int -> Bool
prop_posicion1 (NonNegative x) =
  posicion5 (x,0,0) == x * (x^2 + 6*x + 11) `div` 6

-- Su comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion1
--    +++ OK, passed 100 tests.

-- La 2ª propiedad es
prop_posicion2 :: NonNegative Int -> Bool
prop_posicion2 (NonNegative y) =
  posicion5 (0,y,0) == y * (y^2 + 3*y + 8) `div` 6

-- Su comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion2
--    +++ OK, passed 100 tests.

-- La 3ª propiedad es
prop_posicion3 :: NonNegative Int -> Bool
prop_posicion3 (NonNegative z) =
  posicion5 (0,0,z) == z * (z^2 + 3*z + 2) `div` 6

-- Su comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion3
--    +++ OK, passed 100 tests.

-- La 4ª propiedad es
prop_posicion4 :: NonNegative Int -> Bool
prop_posicion4 (NonNegative x) =
  posicion5 (x,x,x) == x * (9 * x^2 + 14 * x + 7) `div` 2

-- Su comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion4
--    +++ OK, passed 100 tests.
