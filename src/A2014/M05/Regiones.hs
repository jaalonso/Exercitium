-- Regiones.hs
-- Regiones determinadas por n rectas del plano.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-mayo-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En los siguientes dibujos se observa que el número máximo de regiones
-- en el plano generadas con 1, 2 ó 3 líneas son 2, 4 ó 7,
-- respectivamente.
--
--                       \  |
--                        \5|
--                         \|
--                          \
--                          |\
--                          | \
--                |         |  \
--     1        1 | 3     1 | 3 \  6
--    ------   ---|---   ---|----\---
--     2        2 | 4     2 | 4   \ 7
--                |         |      \
--
-- Definir la función
--    regiones :: Integer -> Integer
-- tal que (regiones n) es el número máximo de regiones en el plano
-- generadas con n líneas. Por ejemplo,
--    regiones 1     ==  2
--    regiones 2     ==  4
--    regiones 3     ==  7
--    regiones 100   ==  5051
--    regiones 1000  ==  500501
--    regiones 10000 ==  50005001
--    length (show (regiones (10^(10^5)))) ==  200000
--    length (show (regiones (10^(10^6)))) ==  2000000
--    length (show (regiones (10^(10^7)))) ==  20000000
-- ---------------------------------------------------------------------

module A2014.M05.Regiones where

import Data.List (foldl', genericIndex)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

regiones1 :: Integer -> Integer
regiones1 0 = 1
regiones1 n = regiones1 (n-1) + n

-- 2ª solución
-- ===========

regiones2 :: Integer -> Integer
regiones2 n = 1 + sum [0..n]

-- 3ª solución
-- ===========

regiones3 :: Integer -> Integer
regiones3 n = foldl' (+) 1 [1..n]

-- 4ª solución
-- ===========

regiones4 :: Integer -> Integer
regiones4 n = 1 + sumas `genericIndex` n

-- (sumas n) es la suma 0 + 1 + 2 +...+ n. Por ejemplo,
--    take 10 sumas  ==  [0,1,3,6,10,15,21,28,36,45]
sumas :: [Integer]
sumas = scanl1 (+) [0..]

-- 5ª solución
-- ===========

regiones5 :: Integer -> Integer
regiones5 n = 1 + n*(n+1) `div` 2

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer) -> Spec
specG regiones = do
  it "e1" $
    regiones 1     `shouldBe`  2
  it "e2" $
    regiones 2     `shouldBe`  4
  it "e3" $
    regiones 3     `shouldBe`  7
  it "e4" $
    regiones 100   `shouldBe`  5051
  it "e5" $
    regiones 1000  `shouldBe`  500501
  it "e6" $
    regiones 10000 `shouldBe`  50005001

spec :: Spec
spec = do
  describe "def. 1" $ specG regiones1
  describe "def. 2" $ specG regiones2
  describe "def. 3" $ specG regiones3
  describe "def. 4" $ specG regiones4
  describe "def. 5" $ specG regiones5

-- La verificación es
--    λ> verifica
--    30 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_regiones :: Positive Integer -> Bool
prop_regiones (Positive n) =
  all (== regiones1 n)
      [regiones2 n,
       regiones3 n,
       regiones4 n,
       regiones5 n]

-- La comprobación es
--    λ> quickCheck prop_regiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> regiones1 (4*10^6)
--    8000002000001
--    (2.58 secs, 938,555,088 bytes)
--    λ> regiones2 (4*10^6)
--    8000002000001
--    (0.27 secs, 352,606,080 bytes)
--    λ> regiones3 (4*10^6)
--    8000002000001
--    (0.15 secs, 352,605,944 bytes)
--    λ> regiones4 (4*10^6)
--    8000002000001
--    (1.30 secs, 1,413,495,088 bytes)
--    λ> regiones5 (4*10^6)
--    8000002000001
--    (0.01 secs, 606,072 bytes)
