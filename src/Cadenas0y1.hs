-- Cadenas0y1.hs
-- Cadenas de ceros y unos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-Julio-2014 (actualizado 8-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la constante
--    cadenasCerosUnos :: [String]
-- tal que cadenasCerosUnos es la lista de cadenas de ceros y unos,
-- ordenada lexicográficamente. Por ejemplo,
--    λ> take 15 cadenasCerosUnos1
--    ["","0","1","00","01","10","11","000","001","010","011","100",
--     "101","110","111"]
-- ---------------------------------------------------------------------

module Cadenas0y1 where

import Control.Monad (replicateM)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

cadenasCerosUnos1 :: [String]
cadenasCerosUnos1 =
  concatMap cadenasLongitud1 [0..]

-- (cadenasLongitud1 n) es la lista de cadenas de loongitud n de ceros y
-- unos, ordenada lexicográficamente. Por ejemplo,
--    λ> cadenasLongitud1 3
--    ["000","001","010","011","100","101","110","111"]
cadenasLongitud1 :: Int -> [String]
cadenasLongitud1 0 = [""]
cadenasLongitud1 n = [ c : s | c <- ['0', '1'], s <- cadenasLongitud1 (n - 1) ]

-- 2ª solución
-- ===========

cadenasCerosUnos2 :: [String]
cadenasCerosUnos2 =
  concatMap cadenasLongitud2 [0..]

cadenasLongitud2 :: Int -> [String]
cadenasLongitud2 n =
  sequence (replicate n ['0', '1'])

-- 3ª solución
-- ===========

cadenasCerosUnos3 :: [String]
cadenasCerosUnos3 =
  concatMap cadenasLongitud3 [0..]

cadenasLongitud3 :: Int -> [String]
cadenasLongitud3 n =
  replicateM n ['0', '1']

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [String] -> Spec
specG cadenasCerosUnos = do
  it "e1" $
    take 15 cadenasCerosUnos `shouldBe`
    ["","0","1","00","01","10","11","000","001","010","011","100","101","110","111"]

spec :: Spec
spec = do
  describe "def. 1" $ specG cadenasCerosUnos1
  describe "def. 2" $ specG cadenasCerosUnos2
  describe "def. 3" $ specG cadenasCerosUnos3

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_cadenasCerosUnos :: NonNegative Int -> Bool
prop_cadenasCerosUnos (NonNegative n) =
  all (== cadenasCerosUnos1 !! n)
      [ cadenasCerosUnos2 !! n
      , cadenasCerosUnos3 !! n
      ]

-- La comprobación es
--    λ> quickCheck prop_cadenasCerosUnos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> cadenasCerosUnos1 !! 1000000
--    "1110100001001000001"
--    (5.85 secs, 2,655,311,384 bytes)
--    λ> cadenasCerosUnos2 !! 1000000
--    "1110100001001000001"
--    (0.10 secs, 253,292,456 bytes)
--    λ> cadenasCerosUnos3 !! 1000000
--    "1110100001001000001"
--    (0.09 secs, 253,282,368 bytes)
