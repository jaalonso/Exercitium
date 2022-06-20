-- Ceros_finales_del_factorial.hs
-- Ceros finales del factorial.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    cerosDelFactorial :: Integer -> Integer
-- tal que (cerosDelFactorial n) es el número de ceros en que termina el
-- factorial de n. Por ejemplo,  
--    cerosDelFactorial 24                         == 4
--    cerosDelFactorial 25                         == 6
--    length (show (cerosDelFactorial (10^70000))) == 70000
-- ---------------------------------------------------------------------

module Ceros_finales_del_factorial where

import Test.Hspec                -- Para verificación
import Test.QuickCheck           -- Para verificación

import Data.List (genericLength)

-- 1ª solución
-- ===========

cerosDelFactorial1 :: Integer -> Integer
cerosDelFactorial1 n = ceros (factorial n)

-- (factorial n) es el factorial n. Por ejemplo,
--    factorial 3  ==  6
factorial :: Integer -> Integer
factorial n = product [1..n]

-- (ceros n) es el número de ceros en los que termina el número n. Por
-- ejemplo, 
--    ceros 320000  ==  4
ceros :: Integer -> Integer
ceros n | rem n 10 /= 0 = 0
        | otherwise     = 1 + ceros (div n 10)

-- 2ª solución
-- ===========

cerosDelFactorial2 :: Integer -> Integer
cerosDelFactorial2 = ceros2 . factorial 

ceros2 :: Integer -> Integer
ceros2 n = genericLength (takeWhile (=='0') (reverse (show n)))

-- 3ª solución
-- =============

cerosDelFactorial3 :: Integer -> Integer
cerosDelFactorial3 n
  | n < 5     = 0
  | otherwise = m + cerosDelFactorial3 m
  where m = n `div` 5

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_cerosDelFactorial :: Positive Integer -> Bool
prop_cerosDelFactorial (Positive n) =
  all (== cerosDelFactorial1 n)
      [cerosDelFactorial2 n,
       cerosDelFactorial3 n]

-- La comprobación es
--    λ> quickCheck prop_cerosDelFactorial
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es


-- Comparación de la eficiencia
--    λ> cerosDelFactorial1 (4*10^4)
--    9998
--    (1.93 secs, 2,296,317,904 bytes)
--    λ> cerosDelFactorial2 (4*10^4)
--    9998
--    (1.57 secs, 1,636,242,040 bytes)
--    λ> cerosDelFactorial3 (4*10^4)
--    9998
--    (0.02 secs, 527,584 bytes)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica :: (Integer -> Integer) -> IO ()
verifica f = hspec $ do
  it "e1" $
    cerosDelFactorial 24 `shouldBe`  4 
  it "e2" $
    cerosDelFactorial 25 `shouldBe`  6
  it "p1" $ property $
    \x -> x >= 0 ==> cerosDelFactorial x == cerosDelFactorial3 x
  where cerosDelFactorial = f
