-- Primos_consecutivos_con_media_capicua.hs
-- Primos consecutivos con media capicúa.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-enero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la lista
--    primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
-- formada por las ternas (x,y,z) tales que x e y son primos
-- consecutivos cuya media, z, es capicúa. Por ejemplo,
--    λ> take 5 primosConsecutivosConMediaCapicua
--    [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
--    λ> primosConsecutivosConMediaCapicua !! 500
--    (5687863,5687867,5687865)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Primos_consecutivos_con_media_capicua where

import Data.List (genericTake)
import Data.Numbers.Primes (primes)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

primosConsecutivosConMediaCapicua1 :: [(Integer,Integer,Integer)]
primosConsecutivosConMediaCapicua1 =
  [(x,y,z) | (x,y) <- zip primosImpares (tail primosImpares),
             let z = (x + y) `div` 2,
             capicua z]

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

-- primosImpares es la lista de los números primos impares. Por ejemplo,
--    take 10 primosImpares  ==  [3,5,7,11,13,17,19,23,29]
primosImpares :: [Integer]
primosImpares = [x | x <- [3,5..], primo x]

-- (capicua x) se verifica si x es capicúa. Por ejemplo,
--    capicua 232  == True
--    capicua 223  == False
capicua :: Integer -> Bool
capicua x = ys == reverse ys
  where ys = show x

-- 2ª solución
-- ===========

primosConsecutivosConMediaCapicua2 :: [(Integer,Integer,Integer)]
primosConsecutivosConMediaCapicua2 =
  [(x,y,z) | (x,y) <- zip primosImpares2 (tail primosImpares2),
             let z = (x + y) `div` 2,
             capicua z]

primosImpares2 :: [Integer]
primosImpares2 = tail (criba [2..])
  where criba (p:ps) = p : criba [n | n <- ps, mod n p /= 0]

-- 3ª solución
-- ===========

primosConsecutivosConMediaCapicua3 :: [(Integer,Integer,Integer)]
primosConsecutivosConMediaCapicua3 =
  [(x,y,z) | (x,y) <- zip (tail primos3) (drop 2 primos3),
             let z = (x + y) `div` 2,
             capicua z]

primos3 :: [Integer]
primos3 = 2 : 3 : criba3 0 (tail primos3) 3
  where criba3 k (p:ps) x = [n | n <- [x+2,x+4..p*p-2],
                                 and [n `rem` q /= 0 | q <- take k (tail primos3)]]
                            ++ criba3 (k+1) ps (p*p)

-- 4ª solución
-- ===========

primosConsecutivosConMediaCapicua4 :: [(Integer,Integer,Integer)]
primosConsecutivosConMediaCapicua4 =
  [(x,y,z) | (x,y) <- zip (tail primes) (drop 2 primes),
             let z = (x + y) `div` 2,
             capicua z]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [(Integer,Integer,Integer)] -> Spec
specG primosConsecutivosConMediaCapicua = do
  it "e1" $
    take 5 primosConsecutivosConMediaCapicua `shouldBe`
    [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]

spec :: Spec
spec = do
  describe "def. 1" $ specG primosConsecutivosConMediaCapicua1
  describe "def. 2" $ specG primosConsecutivosConMediaCapicua2
  describe "def. 3" $ specG primosConsecutivosConMediaCapicua3
  describe "def. 4" $ specG primosConsecutivosConMediaCapicua4

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Equivalencia de definiciones
-- ============================

-- La propiedad es
prop_primosConsecutivosConMediaCapicua :: Integer -> Bool
prop_primosConsecutivosConMediaCapicua n =
  all (== genericTake n primosConsecutivosConMediaCapicua1)
      [genericTake n primosConsecutivosConMediaCapicua2,
       genericTake n primosConsecutivosConMediaCapicua3,
       genericTake n primosConsecutivosConMediaCapicua4]

-- La comprobación es
--    λ> prop_primosConsecutivosConMediaCapicua 25 {-# SCC "" #-}
--    True

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> primosConsecutivosConMediaCapicua !! 30
--    (12919,12923,12921)
--    (4.60 secs, 1,877,064,288 bytes)
--    λ> primosConsecutivosConMediaCapicua2 !! 30
--    (12919,12923,12921)
--    (0.69 secs, 407,055,848 bytes)
--    λ> primosConsecutivosConMediaCapicua3 !! 30
--    (12919,12923,12921)
--    (0.07 secs, 18,597,104 bytes)
--    λ> primosConsecutivosConMediaCapicua4 !! 30
--    (12919,12923,12921)
--    (0.01 secs, 10,065,784 bytes)
--
--    λ> primosConsecutivosConMediaCapicua2 !! 40
--    (29287,29297,29292)
--    (2.67 secs, 1,775,554,576 bytes)
--    λ> primosConsecutivosConMediaCapicua3 !! 40
--    (29287,29297,29292)
--    (0.09 secs, 32,325,808 bytes)
--    λ> primosConsecutivosConMediaCapicua4 !! 40
--    (29287,29297,29292)
--    (0.01 secs, 22,160,072 bytes)
--
--    λ> primosConsecutivosConMediaCapicua3 !! 150
--    (605503,605509,605506)
--    (3.68 secs, 2,298,403,864 bytes)
--    λ> primosConsecutivosConMediaCapicua4 !! 150
--    (605503,605509,605506)
--    (0.24 secs, 491,917,240 bytes)
