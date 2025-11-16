-- Repeticion_ciclica.hs
-- Repetición cíclica.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-Noviembre-2014 (actualizado 16-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    ciclica :: [a] -> [a]
-- tal que (ciclica xs) es la lista obtenida repitiendo cíclicamente los
-- elementos de la lista no vacía xs. Por ejemplo,
--    take 10 (ciclica [3,5])    ==  [3,5,3,5,3,5,3,5,3,5]
--    take 10 (ciclica [3,5,7])  ==  [3,5,7,3,5,7,3,5,7,3]
--    take 10 (ciclica [3,5..])  ==  [3,5,7,9,11,13,15,17,19,21]
-- ---------------------------------------------------------------------

module Repeticion_ciclica where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

ciclica1 :: [a] -> [a]
ciclica1 xs = xs ++ ciclica1 xs

-- 2ª solución
-- ===========

ciclica2 :: [a] -> [a]
ciclica2 xs = xs'
  where xs' = xs ++ xs'

-- 3ª solución
-- ===========

ciclica3 :: [a] -> [a]
ciclica3 = concat . repeat

-- 4ª solución
-- ===========

ciclica4 :: [a] -> [a]
ciclica4 = cycle

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int]) -> Spec
specG ciclica = do
  it "e1" $
    take 10 (ciclica [3,5])   `shouldBe` [3,5,3,5,3,5,3,5,3,5]
  it "e2" $
    take 10 (ciclica [3,5,7]) `shouldBe` [3,5,7,3,5,7,3,5,7,3]
  it "e3" $
    take 10 (ciclica [3,5..]) `shouldBe` [3,5,7,9,11,13,15,17,19,21]

spec :: Spec
spec = do
  describe "def. 1" $ specG ciclica1
  describe "def. 2" $ specG ciclica2
  describe "def. 3" $ specG ciclica3
  describe "def. 4" $ specG ciclica4

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Int -> NonEmptyList [Int] -> Bool
prop_equivalencia (NonNegative n) (NonEmpty xs) =
  all (== take n (ciclica1 xs))
      [take n (ciclica2 xs),
       take n (ciclica3 xs),
       take n (ciclica4 xs)]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (take (3*10^7) (ciclica1 [1,2]))
--    2
--    (2.13 secs, 3,960,597,096 bytes)
--    λ> last (take (3*10^7) (ciclica2 [1,2]))
--    2
--    (0.33 secs, 1,680,597,272 bytes)
--    λ> last (take (3*10^7) (ciclica3 [1,2]))
--    2
--    (0.65 secs, 3,840,597,192 bytes)
--    λ> last (take (3*10^7) (ciclica3 [1,2]))
--    2
--    (0.29 secs, 1,680,597,240 bytes)
