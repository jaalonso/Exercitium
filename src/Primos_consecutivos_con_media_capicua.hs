-- Primos_consecutivos_con_media_capicua.hs
-- Primos consecutivos con media capicúa.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-Abril-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la lista
--    primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
-- formada por las ternas (x,y,z) tales que x e y son primos
-- consecutivos cuya media, z, es capicúa. Por ejemplo,
--    λ> take 5 primosConsecutivosConMediaCapicua
--    [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
--
-- Calcular cuántos hay anteriores a 2014.
-- ---------------------------------------------------------------------

module Primos_consecutivos_con_media_capicua where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Data.Numbers.Primes (primes)

-- 1ª solución
-- ===========

primosConsecutivosConMediaCapicua1 :: [(Int,Int,Int)]
primosConsecutivosConMediaCapicua1 =
  [(x,y,z) | (x,y) <- zip primos (tail primos),
             let z = (x + y) `div` 2,
             capicua z]

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Int -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

-- primos es la lista de los números primos mayores que 2. Por ejemplo,
--    take 10 primos  ==  [3,5,7,11,13,17,19,23,29]
primos :: [Int]
primos = [x | x <- [3,5..], primo x]

-- (capicua x) se verifica si x es capicúa. Por ejemplo,
capicua :: Int -> Bool
capicua x = ys == reverse ys
  where ys = show x

-- 2ª solución
-- ===========

primosConsecutivosConMediaCapicua2 :: [(Int,Int,Int)]
primosConsecutivosConMediaCapicua2 =
  [(x,y,z) | (x,y) <- zip (tail primes) (drop 2 primes),
             let z = (x + y) `div` 2,
             capicua z]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [(Int,Int,Int)] -> Spec
specG primosConsecutivosConMediaCapicua = do
  it "e1" $
    take 5 primosConsecutivosConMediaCapicua `shouldBe`
    [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]

spec :: Spec
spec = do
  describe "def. 1" $ specG primosConsecutivosConMediaCapicua1
  describe "def. 2" $ specG primosConsecutivosConMediaCapicua2

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

prop_equivalencia :: Int -> Bool
prop_equivalencia n =
  take n primosConsecutivosConMediaCapicua1 == take n primosConsecutivosConMediaCapicua2

-- La comprobación es
--    λ> prop_equivalencia 30
--    True

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> primosConsecutivosConMediaCapicua1 !! 30
--    (12919,12923,12921)
--    (3.16 secs, 1,877,340,488 bytes)
--    λ> primosConsecutivosConMediaCapicua2 !! 30
--    (12919,12923,12921)
--    (0.01 secs, 9,236,008 bytes)

-- Cálculo
-- =======

-- El cálculo es
--    λ> length (takeWhile (\(x,y,z) -> y < 2014) primosConsecutivosConMediaCapicua2)
--    20
