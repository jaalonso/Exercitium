-- Sumas_de_dos_primos.hs
-- Sumas de dos primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la sucesión
--    sumasDeDosPrimos :: [Integer]
-- cuyos elementos son los números que se pueden escribir como suma de
-- dos números primos. Por ejemplo,
--    λ> take 23 sumasDeDosPrimos
--    [4,5,6,7,8,9,10,12,13,14,15,16,18,19,20,21,22,24,25,26,28,30,31]
--    λ> sumasDeDosPrimos !! (5*10^5)
--    862878
-- ---------------------------------------------------------------------

module Sumas_de_dos_primos where

import Data.Numbers.Primes (isPrime, primes)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumasDeDosPrimos1 :: [Integer]
sumasDeDosPrimos1 =
  [n | n <- [1..], not (null (sumaDeDosPrimos1 n))]

-- (sumaDeDosPrimos1 n) es la lista de pares de primos cuya suma es
-- n. Por ejemplo,
--    sumaDeDosPrimos  9  ==  [(2,7),(7,2)]
--    sumaDeDosPrimos 16  ==  [(3,13),(5,11),(11,5),(13,3)]
--    sumaDeDosPrimos 17  ==  []
sumaDeDosPrimos1 :: Integer -> [(Integer,Integer)]
sumaDeDosPrimos1 n =
  [(x,n-x) | x <- primosN, isPrime (n-x)]
  where primosN = takeWhile (< n) primes

-- 2ª solución
-- ===========

sumasDeDosPrimos2 :: [Integer]
sumasDeDosPrimos2 =
  [n | n <- [1..], not (null (sumaDeDosPrimos2 n))]

-- (sumasDeDosPrimos2 n) es la lista de pares (x,y) de primos cuya suma
-- es n y tales que x <= y. Por ejemplo,
--    sumaDeDosPrimos2  9  ==  [(2,7)]
--    sumaDeDosPrimos2 16  ==  [(3,13),(5,11)]
--    sumaDeDosPrimos2 17  ==  []
sumaDeDosPrimos2 :: Integer -> [(Integer,Integer)]
sumaDeDosPrimos2 n =
  [(x,n-x) | x <- primosN, isPrime (n-x)]
  where primosN = takeWhile (<= (n `div` 2)) primes

-- 3ª solución
-- ===========

sumasDeDosPrimos3 :: [Integer]
sumasDeDosPrimos3 = filter esSumaDeDosPrimos3 [4..]

-- (esSumaDeDosPrimos3 n) se verifica si n es suma de dos primos. Por
-- ejemplo,
--    esSumaDeDosPrimos3  9  ==  True
--    esSumaDeDosPrimos3 16  ==  True
--    esSumaDeDosPrimos3 17  ==  False
esSumaDeDosPrimos3 :: Integer -> Bool
esSumaDeDosPrimos3 n
  | odd n     = isPrime (n-2)
  | otherwise = any isPrime [n-x | x <- takeWhile (<= (n `div` 2)) primes]

-- 4ª solución
-- ===========

-- Usando la conjetura de Goldbach que dice que "Todo número par mayor
-- que 2 puede escribirse como suma de dos números primos" .

sumasDeDosPrimos4 :: [Integer]
sumasDeDosPrimos4 = filter esSumaDeDosPrimos4 [4..]

-- (esSumaDeDosPrimos4 n) se verifica si n es suma de dos primos. Por
-- ejemplo,
--    esSumaDeDosPrimos4  9  ==  True
--    esSumaDeDosPrimos4 16  ==  True
--    esSumaDeDosPrimos4 17  ==  False
esSumaDeDosPrimos4 :: Integer -> Bool
esSumaDeDosPrimos4 n = even n || isPrime (n-2)

-- Verificación                                                     --
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Integer] -> Spec
specG sumasDeDosPrimos = do
  it "e1" $
    take 23 sumasDeDosPrimos `shouldBe`
    [4,5,6,7,8,9,10,12,13,14,15,16,18,19,20,21,22,24,25,26,28,30,31]

spec :: Spec
spec = do
  describe "def. 1" $ specG sumasDeDosPrimos1
  describe "def. 2" $ specG sumasDeDosPrimos2
  describe "def. 3" $ specG sumasDeDosPrimos3
  describe "def. 4" $ specG sumasDeDosPrimos4

-- La verificación es
--    λ> verifica
--
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumasDeDosPrimos :: NonNegative Int -> Bool
prop_sumasDeDosPrimos (NonNegative n) =
  all (== sumasDeDosPrimos1 !! n)
      [sumasDeDosPrimos2 !! n,
       sumasDeDosPrimos3 !! n,
       sumasDeDosPrimos4 !! n]

-- La comprobación es
--    λ> quickCheck prop_sumasDeDosPrimos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumasDeDosPrimos1 !! 5000
--    7994
--    (2.61 secs, 9,299,106,792 bytes)
--    λ> sumasDeDosPrimos2 !! 5000
--    7994
--    (1.48 secs, 5,190,651,760 bytes)
--    λ> sumasDeDosPrimos3 !! 5000
--    7994
--    (0.12 secs, 351,667,104 bytes)
--    λ> sumasDeDosPrimos4 !! 5000
--    7994
--    (0.04 secs, 63,464,320 bytes)
--
--    λ> sumasDeDosPrimos3 !! (5*10^4)
--    83674
--    (2.23 secs, 7,776,049,264 bytes)
--    λ> sumasDeDosPrimos4 !! (5*10^4)
--    83674
--    (0.34 secs, 1,183,604,984 bytes)

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- N.J.A. Sloane, "Sucesión A014091" en OEIS http://oeis.org/A014091
