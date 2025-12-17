-- Enumeracion_de_pares_de_naturales.hs
-- Enumeración de los pares de números naturales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-Diciembre-2014 (actualizado 17-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los pares de los números naturales se pueden ordenar por la suma de
-- sus componentes y entres los pares con la misma suma elegir antes al
-- que tiene mayor su primera componente.
--
-- Definir la función
--    pares :: [(Int,Int)]
-- tal que pares es la lista de los pares de números naturales con el
-- orden anterior. por ejemplo,
--    λ> take 10 pares
--    [(0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3)]
--
-- Usando la definición de pares, definir la función
--    posicion :: (Int,Int) -> Int
-- tal que (posicion p) es la posición del par p en la lista pares. Por
-- ejemplo,
--    posicion (0,0)  ==  0
--    posicion (2,0)  ==  3
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Enumeracion_de_pares_de_naturales where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

pares1 :: [(Int,Int)]
pares1 = [(n-y,y) | n <- [0..], y <- [0..n]]

posicion1 :: (Int,Int) -> Int
posicion1 p = length (takeWhile (/=p) pares1)

-- 2ª solución
-- ===========

pares2 :: [(Int,Int)]
pares2 = [(a,n - a) | n <- [0..], a <- [n,n-1..0]]

posicion2 :: (Int,Int) -> Int
posicion2 p = head [i | (x,i) <- zip pares2 [0..], x == p]

-- 3ª solución
-- ===========

posicion3 :: (Int,Int) -> Int
posicion3 (x,y) = y + (x+y+1)*(x+y) `div` 2

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Int,Int) -> Int) -> Spec
specG posicion = do
  it "e1" $
    posicion (0,0) `shouldBe` 0
  it "e2" $
    posicion (2,0) `shouldBe` 3

spec :: Spec
spec = do
  describe "def. 1" $ specG posicion1
  describe "def. 2" $ specG posicion2
  describe "def. 3" $ specG posicion3

-- La verificación es
--    λ> verifica
--    6 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: (NonNegative Int, NonNegative Int) -> Bool
prop_equivalencia (NonNegative x, NonNegative y) =
  all (== posicion1 (x,y))
      [posicion2 (x,y),
       posicion3 (x,y)]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> posicion1 (3000,0)
--    4501500
--    (1.94 secs, 1,297,586,008 bytes)
--    λ> posicion2 (3000,0)
--    4501500
--    (2.46 secs, 1,909,886,352 bytes)
--    λ> posicion3 (3000,0)
--    4501500
--    (0.01 secs, 600,760 bytes)
