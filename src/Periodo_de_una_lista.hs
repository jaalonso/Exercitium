-- Periodo_de_una_lista.hs
-- Período de una lista.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-Diciembre-2014 (actualizado 21-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El período de una lista xs es la lista más corta ys tal que xs se
-- puede obtener concatenando varias veces la lista ys. Por ejemplo, el
-- período "abababab" es "ab" ya que "abababab" se obtiene repitiendo
-- tres veces la lista "ab".
--
-- Definir la función
--    periodo :: Eq a => [a] -> [a]
-- tal que (periodo xs) es el período de xs. Por ejemplo,
--    periodo "ababab"      ==  "ab"
--    periodo "buenobueno"  ==  "bueno"
--    periodo "oooooo"      ==  "o"
--    periodo "sevilla"     ==  "sevilla"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Periodo_de_una_lista where

import Data.List (isPrefixOf, inits)
import Data.List.Split (chunksOf)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

periodo1 :: Eq a => [a] -> [a]
periodo1 xs = head [ys | ys <- tail (inits xs), esPeriodo1 ys xs]

-- (esPeriodo ys xs) se verifica si ys es un periodo de xs. Por ejemplo,
--    esPeriodo "ab" "ababab"  == True
--    esPeriodo "aba" "ababab" == False
esPeriodo1 :: Eq a => [a] -> [a] -> Bool
esPeriodo1 ys xs =
  n `mod` m == 0 &&
  take n (cycle ys) == xs
  where m = length ys
        n = length xs

-- 2ª solución
-- ===========

periodo2 :: Eq a => [a] -> [a]
periodo2 xs = head [ys | ys <- tail (inits xs), esPeriodo2 ys xs]

esPeriodo2 :: Eq a => [a] -> [a] -> Bool
esPeriodo2 ys xs =
  n `mod` m == 0 &&
  xs `isPrefixOf` cycle ys
  where m = length ys
        n = length xs

-- 3ª solución
-- ===========

periodo3 :: Eq a => [a] -> [a]
periodo3 xs = head [ys | ys <- tail (inits xs),
                         all (== ys) (chunksOf (length ys) xs)]

-- 4ª solución
-- ===========

periodo4 :: Eq a => [a] -> [a]
periodo4 xs = head [ys | n <- [1..length xs],
                         length xs `mod` n == 0,
                         let ys = take n xs,
                         take (length xs) (cycle ys) == xs]

-- 5ª solución
-- ===========

periodo5 :: Eq a => [a] -> [a]
periodo5 xs = head [ys | n <- [1..length xs],
                         length xs `mod` n == 0,
                         let ys = take n xs,
                         xs `isPrefixOf` cycle ys]

-- 6ª solución
-- ===========

periodo6 :: Eq a => [a] -> [a]
periodo6 xs = take n xs
  where l = length xs
        n = head [m | m <- divisores l,
                      concat (replicate (l `div` m) (take m xs)) == xs]

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 96  ==  [1,2,3,4,6,8,12,16,24,32,48,96]
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- 7ª solución
-- ===========

periodo7 :: Eq a => [a] -> [a]
periodo7 xs = take n xs
  where l = length xs
        n = head [m | m <- divisores l,
                      xs `isPrefixOf` cycle (take m xs)]

-- 8ª solución
-- ===========

periodo8 :: Eq a => [a] -> [a]
periodo8 xs = buscar 1
  where
    n = length xs
    buscar k
      | n `mod` k == 0 && esRepeticion k = take k xs
      | otherwise = buscar (k + 1)
    esRepeticion k = take (n - k) xs == drop k xs

-- 9ª solución
-- ===========

periodo9 :: Eq a => [a] -> [a]
periodo9 xs = head [take m xs | m <- [1..n],
                                n `mod` m == 0,
                                take (n - m) xs == drop m xs]
  where n = length xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> String) -> Spec
specG periodo = do
  it "e1" $
    periodo "ababab"      `shouldBe` "ab"
  it "e2" $
    periodo "buenobueno"  `shouldBe` "bueno"
  it "e3" $
    periodo "oooooo"      `shouldBe` "o"
  it "e4" $
    periodo "sevilla"     `shouldBe` "sevilla"

spec :: Spec
spec = do
  describe "def. 1" $ specG periodo1
  describe "def. 2" $ specG periodo2
  describe "def. 3" $ specG periodo3
  describe "def. 4" $ specG periodo4
  describe "def. 5" $ specG periodo5
  describe "def. 6" $ specG periodo6
  describe "def. 7" $ specG periodo7
  describe "def. 8" $ specG periodo8
  describe "def. 9" $ specG periodo9

-- La verificación es
--    λ> verifica
--    36 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Generador de un solo carácter alfanumérico. Por ejemplo.
--    λ> generate genCaracter
--    '3'
--    λ> generate genCaracter
--    'G'
genCaracter :: Gen Char
genCaracter = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

-- Generador de una cadena alfanumérica no vacía. Por ejemplo,
--    λ> generate genCadena
--    "c84SjmK8sSuqkNOWCIcT4GRd6a"
--    λ> generate genCadena
--    "l"
--    λ> generate genCadena
--    "zNZsKUpc"
genCadena :: Gen String
genCadena = listOf1 genCaracter

-- La propiedad es
prop_equivalencia :: Property
prop_equivalencia = forAll genCadena $ \xs ->
  all (== periodo1 xs)
      [periodo2 xs,
       periodo3 xs,
       periodo4 xs,
       periodo5 xs,
       periodo6 xs,
       periodo7 xs,
       periodo8 xs,
       periodo9 xs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> ejemplo1 = take (2*10^4) (cycle (take 6000 (concat (map show [1..]))))
--    λ> length (periodo1 ejemplo1)
--    20000
--    (2.19 secs, 9,581,066,176 bytes)
--    λ> length (periodo2 ejemplo1)
--    20000
--    (2.15 secs, 9,577,027,408 bytes)
--    λ> length (periodo3 ejemplo1)
--    20000
--    (5.36 secs, 20,785,721,104 bytes)
--    λ> length (periodo4 ejemplo1)
--    20000
--    (0.65 secs, 13,994,824 bytes)
--    λ> length (periodo5 ejemplo1)
--    20000
--    (0.65 secs, 11,234,320 bytes)
--    λ> length (periodo6 ejemplo1)
--    20000
--    (0.04 secs, 11,248,560 bytes)
--    λ> length (periodo7 ejemplo1)
--    20000
--    (0.04 secs, 11,236,864 bytes)
--    λ> length (periodo8 ejemplo1)
--    20000
--    (0.04 secs, 5,727,632 bytes)
--    λ> length (periodo9 ejemplo1)
--    20000
--    (0.05 secs, 8,765,352 bytes)
--
--    λ> ejemplo2 = take (2*10^6) (cycle (take (product [1..20]) (concat (map show [1..]))))
--    λ> length (periodo6 ejemplo2)
--    2000000
--    (0.94 secs, 1,070,440,856 bytes)
--    λ> length (periodo7 ejemplo2)
--    2000000
--    (1.30 secs, 1,665,979,256 bytes)
--    λ> length (periodo8 ejemplo2)
--    2000000
--    (1.22 secs, 816,612,416 bytes)
--    λ> length (periodo9 ejemplo2)
--    2000000
--    (0.76 secs, 512,616,296 bytes)
