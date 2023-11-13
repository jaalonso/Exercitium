-- Raices_enteras.hs
-- Raíces enteras.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-noviembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    raizEnt :: Integer -> Integer -> Integer
-- tal que (raizEnt x n) es la raíz entera n-ésima de x; es decir, el
-- mayor número entero y tal que y^n <= x. Por ejemplo,
--    raizEnt  8 3      ==  2
--    raizEnt  9 3      ==  2
--    raizEnt 26 3      ==  2
--    raizEnt 27 3      ==  3
--    raizEnt (10^50) 2 ==  10000000000000000000000000
--
-- Comprobar con QuickCheck que para todo número natural n,
--     raizEnt (10^(2*n)) 2 == 10^n
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Raices_enteras where

import Test.Hspec (Spec, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

raizEnt1 :: Integer -> Integer -> Integer
raizEnt1 x n =
  last (takeWhile (\y -> y^n <= x) [0..])

-- 2ª solución
-- ===========

raizEnt2 :: Integer -> Integer -> Integer
raizEnt2 x n =
  floor ((fromIntegral x)**(1 / fromIntegral n))

-- Nota. La solución anterior falla para números grandes. Por ejemplo,
--    λ> raizEnt2 (10^50) 2 == 10^25
--    False

-- 3ª solución
-- ===========

raizEnt3 :: Integer -> Integer -> Integer
raizEnt3 x n = aux (1,x)
  where aux (a,b) | d == x    = c
                  | c == a    = c
                  | d < x     = aux (c,b)
                  | otherwise = aux (a,c)
          where c = (a+b) `div` 2
                d = c^n

-- Comparación de eficiencia
-- =========================

--    λ> raizEnt1 (10^14) 2
--    10000000
--    (6.15 secs, 6,539,367,976 bytes)
--    λ> raizEnt2 (10^14) 2
--    10000000
--    (0.00 secs, 0 bytes)
--    λ> raizEnt3 (10^14) 2
--    10000000
--    (0.00 secs, 25,871,944 bytes)
--
--    λ> raizEnt2 (10^50) 2
--    9999999999999998758486016
--    (0.00 secs, 0 bytes)
--    λ> raizEnt3 (10^50) 2
--    10000000000000000000000000
--    (0.00 secs, 0 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_raizEnt :: Integer -> Bool
prop_raizEnt n =
  raizEnt3 (10^(2*m)) 2 == 10^m
  where m = abs n

-- La comprobación es
--    λ> quickCheck prop_raizEnt
--    +++ OK, passed 100 tests.

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    raizEnt1  8 3 `shouldBe` 2
  it "e2" $
    raizEnt1  9 3 `shouldBe` 2
  it "e3" $
    raizEnt1 26 3 `shouldBe` 2
  it "e4" $
    raizEnt1 27 3 `shouldBe` 3
  it "e5" $
    raizEnt2  8 3 `shouldBe` 2
  it "e6" $
    raizEnt2  9 3 `shouldBe` 2
  it "e7" $
    raizEnt2 26 3 `shouldBe` 2
  it "e8" $
    raizEnt2 27 3 `shouldBe` 3
  it "e9" $
    raizEnt3  8 3 `shouldBe` 2
  it "e10" $
    raizEnt3  9 3 `shouldBe` 2
  it "e11" $
    raizEnt3 26 3 `shouldBe` 2
  it "e12" $
    raizEnt3 27 3 `shouldBe` 3

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--    e6
--    e7
--    e8
--    e9
--    e10
--    e11
--    e12
--
--    Finished in 0.0007 seconds
--    12 examples, 0 failures
