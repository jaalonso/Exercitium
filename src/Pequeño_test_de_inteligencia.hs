-- Pequeño_test_de_inteligencia.hs
-- Pequeño test de inteligencia.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-Noviembre-2014 (actualizado 8-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Recientemente se publicó en la Red un pequeño test de inteligencia
-- cuyo objetivo consistía en descubrir una función a partir de una
-- colección de ejemplos. Los ejemplos eran los siguientes
--    f 6  4 == 210
--    f 9  2 == 711
--    f 8  5 == 313
--    f 5  2 == 37
--    f 7  6 == 113
--    f 9  8 == 117
--    f 10 6 == 416
--    f 15 3 == 1218
--
-- Definir la función
--    f :: Integer -> Integer -> Integer
-- tal que f cubra los ejemplos anteriores y la definición de f sea lo
-- más corta posible (en número de palabras).
-- ---------------------------------------------------------------------

module Pequeño_test_de_inteligencia where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

f1 :: Integer -> Integer -> Integer
f1 x y = read (show (x-y) ++ show (x+y))

-- 2ª solución
-- ===========

f2 :: Integer -> Integer -> Integer
f2 x y = read $ show (x-y) ++ show (x+y)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer -> Integer) -> Spec
specG f = do
  it "e1" $
    f 6  4 `shouldBe` 210
  it "e2" $
    f 9  2 `shouldBe` 711
  it "e3" $
    f 8  5 `shouldBe` 313
  it "e4" $
    f 5  2 `shouldBe` 37
  it "e5" $
    f 7  6 `shouldBe` 113
  it "e6" $
    f 9  8 `shouldBe` 117
  it "e7" $
    f 10 6 `shouldBe` 416
  it "e8" $
    f 15 3 `shouldBe` 1218

spec :: Spec
spec = do
  describe "def. 1" $ specG f1
  describe "def. 2" $ specG f2

-- La verificación es
--    λ> verifica
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Integer -> NonNegative Integer -> Bool
prop_equivalencia (NonNegative x) (NonNegative y) =
  f1 x y == f2 x y

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (f1 (10^1000000) (10^999999)))
--    2000001
--    (0.96 secs, 865,462,080 bytes)
--    λ> length (show (f2 (10^1000000) (10^999999)))
--    2000001
--    (0.93 secs, 865,462,144 bytes)
