-- Suma_de_cadenas.hs
-- Suma de cadenas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-abril-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaCadenas :: String -> String -> String
-- tal que (sumaCadenas xs ys) es la cadena formada por el número entero
-- que es la suma de los números enteros cuyas cadenas que lo
-- representan son xs e ys; además, se supone que la cadena vacía
-- representa al cero. Por ejemplo,
--    sumaCadenas "2"   "6"  == "8"
--    sumaCadenas "14"  "2"  == "16"
--    sumaCadenas "14"  "-5" == "9"
--    sumaCadenas "-14" "-5" == "-19"
--    sumaCadenas "5"   "-5" == "0"
--    sumaCadenas ""    "5"  == "5"
--    sumaCadenas "6"   ""   == "6"
--    sumaCadenas ""    ""   == "0"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Suma_de_cadenas where
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaCadenas1 :: String -> String -> String
sumaCadenas1 xs ys = show (numero xs + numero ys)

-- (numero xs) es el número entero representado por la cadena xs
-- suponiendo que la cadena vacía representa al cero.. Por ejemplo,
--    numero "12"   ==  12
--    numero "-12"  ==  -12
--    numero "0"    ==  0
--    numero ""     ==  0
numero :: String -> Int
numero "" = 0
numero xs = read xs

-- 2ª solución
-- ===========

sumaCadenas2 :: String -> String -> String
sumaCadenas2 "" "" = "0"
sumaCadenas2 "" ys = ys
sumaCadenas2 xs "" = xs
sumaCadenas2 xs ys = show (read xs + read ys)

-- 3ª solución
-- ===========

sumaCadenas3 :: String -> String -> String
sumaCadenas3 xs ys =
  show (sum (map read (filter (not . null) [xs, ys])))

-- 4ª solución
-- ===========

sumaCadenas4 :: String -> String -> String
sumaCadenas4 =
  ((show . sum . map read . filter (not . null)) .) . (. return) . (:)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> String -> String) -> Spec
specG sumaCadenas = do
  it "e1" $
    sumaCadenas "2"   "6"  `shouldBe` "8"
  it "e2" $
    sumaCadenas "14"  "2"  `shouldBe` "16"
  it "e3" $
    sumaCadenas "14"  "-5" `shouldBe` "9"
  it "e4" $
    sumaCadenas "-14" "-5" `shouldBe` "-19"
  it "e5" $
    sumaCadenas "5"   "-5" `shouldBe` "0"
  it "e6" $
    sumaCadenas ""    "5"  `shouldBe` "5"
  it "e7" $
    sumaCadenas "6"   ""   `shouldBe` "6"
  it "e8" $
    sumaCadenas ""    ""   `shouldBe` "0"

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaCadenas1
  describe "def. 2" $ specG sumaCadenas2
  describe "def. 3" $ specG sumaCadenas3
  describe "def. 4" $ specG sumaCadenas4

-- La verificación es
--    λ> verifica
--
--    32 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_sumaCadenas :: Int -> Int -> Bool
prop_sumaCadenas x y =
  all (== sumaCadenas1 xs ys)
      [sumaCadenas2 xs ys,
       sumaCadenas3 xs ys,
       sumaCadenas4 xs ys]
  where xs = show x
        ys = show y

-- La comparación es
--    λ> quickCheck prop_sumaCadenas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaCadenas1 (show (10^10000)) ('-' : show (10^10000))
--    "0"
--    (0.03 secs, 7,814,456 bytes)
--    λ> sumaCadenas2 (show (10^10000)) ('-' : show (10^10000))
--    "0"
--    (0.03 secs, 7,814,360 bytes)
--    λ> sumaCadenas3 (show (10^10000)) ('-' : show (10^10000))
--    "0"
--    (0.03 secs, 7,814,936 bytes)
--    λ> sumaCadenas4 (show (10^10000)) ('-' : show (10^10000))
--    "0"
--    (0.03 secs, 7,814,872 bytes)
