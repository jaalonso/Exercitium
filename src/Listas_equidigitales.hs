-- Listas_equidigitales.hs
-- Listas equidigitales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-Octubre-2014 (actualizado 21-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una lista de números naturales es equidigital si todos sus elementos
-- tienen el mismo número de dígitos.
--
-- Definir la función
--    equidigital :: [Int] -> Bool
-- tal que (equidigital xs) se verifica si xs es una lista equidigital.
-- Por ejemplo,
--    equidigital [343,225,777,943]   ==  True
--    equidigital [343,225,777,94,3]  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Listas_equidigitales where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

equidigital1 :: [Int] -> Bool
equidigital1 []     = True
equidigital1 (x:xs) = and [nCifras1 y == n | y <- xs]
  where n = nCifras1 x

-- (nCifras x) es el número de cifras de x. Por ejemplo,
--    nCifras 475  ==  3
nCifras1 :: Int -> Int
nCifras1 x = length (show x)

-- 2ª solución
-- ===========

equidigital2 :: [Int] -> Bool
equidigital2 (x:y:zs) = nCifras1 x == nCifras1 y && equidigital2 (y:zs)
equidigital2 _        = True

-- 3ª solución
-- ===========

equidigital3 :: [Int] -> Bool
equidigital3 []     = True
equidigital3 (x:xs) = aux xs
  where n = nCifras1 x
        aux []     = True
        aux (y:ys) = nCifras1 y == n && aux ys

-- 4ª solución
-- ===========

equidigital4 :: [Int] -> Bool
equidigital4 []     = True
equidigital4 (x:xs) = all (== n) (map nCifras2 xs)
  where n = nCifras2 x

-- (nCifras x) es el número de cifras de x. Por ejemplo,
--    nCifras 475  ==  3
nCifras2 :: Int -> Int
nCifras2 = length . show

-- 5ª solución
-- ===========

equidigital5 :: [Int] -> Bool
equidigital5 []     = True
equidigital5 (x:xs) = all ((== n). nCifras2) xs
  where n = nCifras2 x

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> Bool) -> Spec
specG equidigital = do
  it "e1" $
    equidigital [343,225,777,943]   `shouldBe`  True
  it "e2" $
    equidigital [343,225,777,94,3]  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG equidigital1
  describe "def. 2" $ specG equidigital2
  describe "def. 3" $ specG equidigital3
  describe "def. 4" $ specG equidigital4
  describe "def. 5" $ specG equidigital5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equidigital :: [Int] -> Bool
prop_equidigital xs =
  all (== equidigital1 xs')
      [equidigital2 xs',
       equidigital3 xs',
       equidigital4 xs']
  where xs' = map abs xs

-- La comprobación es
--    λ> quickCheck prop_equidigital
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> equidigital1 [10^6..5*10^6]
--    True
--    (2.39 secs, 2,624,601,480 bytes)
--    λ> equidigital2 [10^6..5*10^6]
--    True
--    (4.75 secs, 4,192,599,904 bytes)
--    λ> equidigital3 [10^6..5*10^6]
--    True
--    (2.75 secs, 2,432,600,368 bytes)
--    λ> equidigital4 [10^6..5*10^6]
--    True
--    (0.78 secs, 2,304,600,600 bytes)
--    λ> equidigital5 [10^6..5*10^6]
--    True
--    (0.74 secs, 2,080,600,536 bytes)
