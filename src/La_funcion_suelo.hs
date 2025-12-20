-- La_funcion_suelo.hs
-- La función suelo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-Diciembre-2014 (actualizado 18-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La función suelo asigna a cada número real el número entero más
-- próximo por defecto; es decir, el mayor número entero igual o menor
-- que ese número real. Por ejemplo, al -2.4 le asigna el -3 y al 1.7 el
-- 1.
--
-- Haskell tiene una implementación de la función suelo llamada
-- floor. El objetivo de este ejercicio es redefinir dicha función; es
-- decir, definir la función
--    suelo :: Float -> Integer
-- tal que (suelo x) es el suelo de x. Por ejemplo,
--    suelo (-2.4)  ==  -3
--    suelo (-2.7)  ==  -3
--    suelo (-2)    ==  -2
--    suelo   2.4   ==  2
--    suelo   2.7   ==  2
--    suelo   2     ==  2
--
-- Comprobar con QuickCheck que las funciones suelo y floor son
-- equivalentes.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module La_funcion_suelo where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

suelo1 :: Float -> Integer
suelo1 x | x < 0     = head [m | m <- [-1,-2..], fromIntegral m <= x]
         | otherwise = head [m | m <- [1..], x < fromIntegral m] - 1

-- 2ª solución
-- ===========

suelo2 :: Float -> Integer
suelo2 x | x < 0     = until (`menorI` x) (subtract 1) (-1)
         | otherwise = until (x `menor`) (+1) 1 - 1
  where menorI m y = fromIntegral m <= y
        menor  y m = y < fromIntegral m

-- 3ª solución
-- ===========

suelo3 :: Float -> Integer
suelo3 x | x < 0     = until ((<=x) . fromIntegral) (subtract 1) (-1)
         | otherwise = until ((x<)  . fromIntegral) (+1) 1 - 1

-- 4ª solución
-- ===========

suelo4 :: Float -> Integer
suelo4 x = if r < 0 then n-1 else n
  where (n,r) = properFraction x

-- 5ª solución
-- ===========

suelo5 :: Float -> Integer
suelo5 x = fst (until unitario (mejora x) (acota x))
    where inferior y     = until (`menorI` y) (*2) (-1)
          superior y     = until (y `menor`) (*2) 1
          menorI m y     = fromIntegral m <= y
          menor  y m     = y < fromIntegral m
          acota y        = (inferior y, superior y)
          mejora y (m,n) = if p `menorI` y then (p,n) else (m,p)
                           where p =(m+n) `div` 2
          unitario (m,n) = (m+1 == n)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Float -> Integer) -> Spec
specG suelo = do
  it "e1" $
    suelo (-2.4)  `shouldBe`  -3
  it "e2" $
    suelo (-2.7)  `shouldBe`  -3
  it "e3" $
    suelo (-2)    `shouldBe`  -2
  it "e4" $
    suelo   2.4   `shouldBe`  2
  it "e5" $
    suelo   2.7   `shouldBe`  2
  it "e6" $
    suelo   2     `shouldBe`  2

spec :: Spec
spec = do
  describe "def. 1" $ specG suelo1
  describe "def. 2" $ specG suelo2
  describe "def. 3" $ specG suelo3
  describe "def. 4" $ specG suelo4
  describe "def. 5" $ specG suelo5

-- La verificación es
--    λ> verifica
--    36 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Float -> Bool
prop_equivalencia x =
  all (== suelo1 x)
      [suelo2 x,
       suelo3 x,
       suelo4 x,
       suelo5 x]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> suelo1 (10^7)
--    10000000
--    (2.93 secs, 2,080,602,576 bytes)
--    λ> suelo2 (10^7)
--    10000000
--    (2.95 secs, 1,520,602,592 bytes)
--    λ> suelo3 (10^7)
--    10000000
--    (1.99 secs, 1,280,602,696 bytes)
--    λ> suelo4 (10^7)
--    10000000
--    (0.01 secs, 602,472 bytes)
--    λ> suelo5 (10^7)
--    10000000
--    (0.00 secs, 628,720 bytes)
