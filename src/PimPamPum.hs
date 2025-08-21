-- PimPamPum.hs
-- Pim, Pam, Pum y divisibilidad.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 2-junio-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sonido :: Int -> String
-- tal que (sonido n) escribe "Pim" si n es divisible por 3, además
-- escribe "Pam" si n es divisible por 5 y también escribe "Pum" si n es
-- divisible por 7. Por ejemplo,
--    sonido   3  ==  "Pim"
--    sonido   5  ==  "Pam"
--    sonido   7  ==  "Pum"
--    sonido   8  ==  ""
--    sonido   9  ==  "Pim"
--    sonido  15  ==  "PimPam"
--    sonido  21  ==  "PimPum"
--    sonido  35  ==  "PamPum"
--    sonido 105  ==  "PimPamPum"
-- ---------------------------------------------------------------------

module PimPamPum where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sonido1 :: Int -> String
sonido1 x = concat [z | (n,z) <- zs, n == 0]
  where xs = [rem x 3, rem x 5, rem x 7]
        zs = zip xs ["Pim","Pam","Pum"]

-- 2ª solución
-- ===========

sonido2 :: Int -> String
sonido2 n = concat (["Pim" | rem n 3 == 0] ++
                    ["Pam" | rem n 5 == 0] ++
                    ["Pum" | rem n 7 == 0])

-- 3ª solución
-- ===========

sonido3 :: Int -> String
sonido3 n = f 3 "Pim" ++ f 5 "Pam" ++ f 7 "Pum"
  where f x c = if rem n x == 0
                then c
                else ""

-- 4ª solución
-- ===========

sonido4 :: Int -> String
sonido4 n =
  concat [s | (s,d) <- zip ["Pim","Pam","Pum"] [3,5,7],
              rem n d == 0]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> String) -> Spec
specG sonido = do
  it "e1" $
    sonido   3  `shouldBe`  "Pim"
  it "e2" $
    sonido   5  `shouldBe`  "Pam"
  it "e3" $
    sonido   7  `shouldBe`  "Pum"
  it "e4" $
    sonido   8  `shouldBe`  ""
  it "e5" $
    sonido   9  `shouldBe`  "Pim"
  it "e6" $
    sonido  15  `shouldBe`  "PimPam"
  it "e7" $
    sonido  21  `shouldBe`  "PimPum"
  it "e8" $
    sonido  35  `shouldBe`  "PamPum"
  it "e9" $
    sonido 105  `shouldBe`  "PimPamPum"

spec :: Spec
spec = do
  describe "def. 1" $ specG sonido1
  describe "def. 2" $ specG sonido2
  describe "def. 3" $ specG sonido3
  describe "def. 4" $ specG sonido4

-- La verificación es
--    λ> verifica
--    36 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sonido :: Positive Int -> Bool
prop_sonido (Positive n) =
  all (== sonido1 n)
      [sonido2 n,
       sonido3 n,
       sonido4 n]

-- La comprobación es
--    λ> quickCheck prop_sonido
--    +++ OK, passed 100 tests.
