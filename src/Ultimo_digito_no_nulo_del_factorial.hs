-- Ultimo_digito_no_nulo_del_factorial.hs
-- Último dígito no nulo del factorial.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-Noviembre-2014 (actualizado 5-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El factorial de 7 es
--    7! = 1 * 2 * 3 * 4 * 5 * 6 * 7 = 5040
-- por tanto, el último dígito no nulo del factorial de 7 es 4.
--
-- Definir la función
--    ultimoNoNuloFactorial :: Integer -> Integer
-- tal que (ultimoNoNuloFactorial n) es el último dígito no nulo del
-- factorial de n. Por ejemplo,
--    ultimoNoNuloFactorial  7  == 4
--    ultimoNoNuloFactorial 10  == 8
--    ultimoNoNuloFactorial 12  == 6
--    ultimoNoNuloFactorial 97  == 2
--    ultimoNoNuloFactorial  0  == 1
--
-- Comprobar con QuickCheck que si n es mayor que 4, entonces el último
-- dígito no nulo del factorial de n es par.
-- ---------------------------------------------------------------------

module Ultimo_digito_no_nulo_del_factorial where

import Data.Char (digitToInt)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

ultimoNoNuloFactorial1 :: Integer -> Integer
ultimoNoNuloFactorial1 = ultimoNoNulo . factorial

-- (factorial n) es el factorial de n. Por ejemplo,
--    factorial 7  ==  5040
factorial :: Integer -> Integer
factorial n = product [1..n]

-- (ultimoNoNulo n) es el último dígito no nulo de n. Por ejemplo,
--    ultimoNoNulo 5040  ==  4
ultimoNoNulo :: Integer -> Integer
ultimoNoNulo n | r /= 0    = r
               | otherwise = ultimoNoNulo q
  where (q,r) = n `quotRem` 10

-- 2ª solución
-- ===========

ultimoNoNuloFactorial2 :: Integer -> Integer
ultimoNoNuloFactorial2 = last . filter (/= 0) . digitos . factorial

digitos :: Integer -> [Integer]
digitos n = [read [x] | x <- show n]

-- 3ª solución
-- ===========

ultimoNoNuloFactorial3 :: Integer -> Integer
ultimoNoNuloFactorial3 = last . filter (/= 0) . digitos3 . factorial3

digitos3 :: Integer -> [Integer]
digitos3 = map (fromIntegral . digitToInt) . show

factorial3 :: Integer -> Integer
factorial3 = product . enumFromTo 1

-- 4ª solución
-- ===========

ultimoNoNuloFactorial4 :: Integer -> Integer
ultimoNoNuloFactorial4 n =
  read [head (dropWhile (=='0') (reverse (show (factorial n))))]

-- 5ª solución
-- ===========

ultimoNoNuloFactorial5 :: Integer -> Integer
ultimoNoNuloFactorial5 =
  read . return . head . dropWhile ('0' ==) . reverse . show . factorial

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer) -> Spec
specG ultimoNoNuloFactorial = do
  it "e1" $
    ultimoNoNuloFactorial  7  `shouldBe` 4
  it "e2" $
    ultimoNoNuloFactorial 10  `shouldBe` 8
  it "e3" $
    ultimoNoNuloFactorial 12  `shouldBe` 6
  it "e4" $
    ultimoNoNuloFactorial 97  `shouldBe` 2
  it "e5" $
    ultimoNoNuloFactorial  0  `shouldBe` 1

spec :: Spec
spec = do
  describe "def. 1" $ specG ultimoNoNuloFactorial1
  describe "def. 2" $ specG ultimoNoNuloFactorial2
  describe "def. 3" $ specG ultimoNoNuloFactorial3
  describe "def. 4" $ specG ultimoNoNuloFactorial4
  describe "def. 5" $ specG ultimoNoNuloFactorial5

-- La verificación es
--    λ> verifica
--    25 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Integer -> Bool
prop_equivalencia (NonNegative n) =
  all (== ultimoNoNuloFactorial1 n)
      [ultimoNoNuloFactorial2 n,
       ultimoNoNuloFactorial3 n,
       ultimoNoNuloFactorial4 n,
       ultimoNoNuloFactorial5 n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ultimoNoNuloFactorial1 100000
--    6
--    (2.86 secs, 13,659,553,384 bytes)
--    λ> ultimoNoNuloFactorial2 100000
--    6
--    (1.44 secs, 10,732,625,568 bytes)
--    λ> ultimoNoNuloFactorial3 100000
--    6
--    (1.08 secs, 9,172,969,128 bytes)
--    λ> ultimoNoNuloFactorial4 100000
--    6
--    (0.99 secs, 9,078,194,656 bytes)
--    λ> ultimoNoNuloFactorial5 100000
--    6
--    (1.00 secs, 9,078,195,296 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_ultimoNoNuloFactorial :: Integer -> Property
prop_ultimoNoNuloFactorial n =
  n > 4 ==> even (ultimoNoNuloFactorial1 n)

-- La comprobación es
--    λ> quickCheck prop_ultimoNoNuloFactorial
--    +++ OK, passed 100 tests.
