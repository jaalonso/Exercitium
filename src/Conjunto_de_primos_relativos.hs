-- Conjunto_de_primos_relativos.hs
-- Conjunto de primos relativos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-Noviembre-2014 (actualizado 5-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Dos números enteros positivos son primos relativos si no tienen
-- ningún factor primo en común; es decir, si 1 es su único divisor
-- común. Por ejemplo, 6 y 35 son primos entre sí, pero 6 y 27 no lo son
-- porque ambos son divisibles por 3.
--
-- Definir la función
--    primosRelativos :: [Int] -> Bool
-- tal que (primosRelativos xs) se verifica si los elementos de xs son
-- primos relativos dos a dos. Por ejemplo,
--    primosRelativos [6,35]         ==  True
--    primosRelativos [6,27]         ==  False
--    primosRelativos [2,3,4]        ==  False
--    primosRelativos [6,35,11]      ==  True
--    primosRelativos [6,35,11,221]  ==  True
--    primosRelativos [6,35,11,231]  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Conjunto_de_primos_relativos where

import Data.Numbers.Primes (primes)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

primosRelativos1 :: [Int] -> Bool
primosRelativos1 xs =
  and [gcd x y == 1 | (x,y) <- pares xs]

-- (pares xs) es la lista de los pares de elementos de xs. Por ejemplo,
--    pares [2,3,4,5]  ==  [(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]
pares :: [a] -> [(a,a)]
pares []     = []
pares (x:xs) = [(x, y) | y <- xs] ++ pares xs

-- 2ª solución
-- ===========

primosRelativos2 :: [Int] -> Bool
primosRelativos2 []     = True
primosRelativos2 (x:xs) =
  and [sonPrimosRelativos x y | y <- xs] && primosRelativos2 xs

-- (sonPrimosRelativos x y) se verifica si x e y son primos
-- relativos. Por ejemplo,
--    sonPrimosRelativos 6 35  ==  True
--    sonPrimosRelativos 6 27  ==  False
sonPrimosRelativos :: Int -> Int -> Bool
sonPrimosRelativos x y =
  gcd x y == 1

-- 3ª solución
-- ===========

primosRelativos3 :: [Int] -> Bool
primosRelativos3 []     = True
primosRelativos3 (x:xs) =
  all (sonPrimosRelativos x) xs && primosRelativos3 xs

-- 4ª solución
-- ===========

primosRelativos4 :: [Int] -> Bool
primosRelativos4 [] = True
primosRelativos4 (x:xs) =
  all ((== 1) . gcd x) xs && primosRelativos4 xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> Bool) -> Spec
specG primosRelativos = do
  it "e1" $
    primosRelativos [6,35]         `shouldBe`  True
  it "e2" $
    primosRelativos [6,27]         `shouldBe`  False
  it "e3" $
    primosRelativos [2,3,4]        `shouldBe`  False
  it "e4" $
    primosRelativos [6,35,11]      `shouldBe`  True
  it "e5" $
    primosRelativos [6,35,11,221]  `shouldBe`  True
  it "e6" $
    primosRelativos [6,35,11,231]  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG primosRelativos1
  describe "def. 2" $ specG primosRelativos2
  describe "def. 3" $ specG primosRelativos3
  describe "def. 4" $ specG primosRelativos4

-- La verificación es
--    λ> verifica
--    24 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_primosRelativos :: [Positive Int] -> Bool
prop_primosRelativos xs =
  all (== primosRelativos1 ys)
      [primosRelativos2 ys,
       primosRelativos3 ys,
       primosRelativos4 ys]
  where ys = getPositive <$> xs

-- La comprobación es
--    λ> quickCheck prop_primosRelativos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ej = take 2000 primes
--    λ> primosRelativos1 ej
--    True
--    (1.65 secs, 2,081,369,936 bytes)
--    λ> primosRelativos2 ej
--    True
--    (1.27 secs, 1,777,761,720 bytes)
--    λ> primosRelativos3 ej
--    True
--    (0.97 secs, 1,537,737,728 bytes)
--    λ> primosRelativos4 ej
--    True
--    (0.76 secs, 1,474,185,496 bytes)
