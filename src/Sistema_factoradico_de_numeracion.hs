-- Sistema_factoradico_de_numeracion.hs
-- Sistema factorádico de numeración.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-abril-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El [sistema factorádico](https://bit.ly/3KQZRue) es un sistema
-- numérico basado en factoriales en el que el n-ésimo dígito, empezando
-- desde la derecha, debe ser multiplicado por n! Por ejemplo, el número
-- "341010" en el sistema factorádico es 463 en el sistema decimal ya
-- que
--    3×5! + 4×4! + 1×3! + 0×2! + 1×1! + 0×0! = 463
--
-- En este sistema numérico, el dígito de más a la derecha es siempre 0,
-- el segundo 0 o 1, el tercero 0,1 o 2 y así sucesivamente.
--
-- Con los dígitos del 0 al 9 el mayor número que podemos codificar es el
-- 10!-1 = 3628799. En cambio, si lo ampliamos con las letras A a Z podemos
-- codificar hasta 36!-1 = 37199332678990121746799944815083519999999910.
--
-- Definir las funciones
--    factoradicoAdecimal :: String -> Integer
--    decimalAfactoradico :: Integer -> String
-- tales que
-- + (factoradicoAdecimal cs) es el número decimal correspondiente al
--   número factorádico cs. Por ejemplo,
--      λ> factoradicoAdecimal "341010"
--      463
--      λ> factoradicoAdecimal "2441000"
--      2022
--      λ> factoradicoAdecimal "A0000000000"
--      36288000
--      λ> map factoradicoAdecimal ["10","100","110","200","210","1000","1010","1100","1110","1200"]
--      [1,2,3,4,5,6,7,8,9,10]
--      λ> factoradicoAdecimal "3KXWVUTSRQPONMLKJIHGFEDCBA9876543210"
--      37199332678990121746799944815083519999999
-- + (decimalAfactoradico n) es el número factorádico correpondiente al
--   número decimal n. Por ejemplo,
--      λ> decimalAfactoradico 463
--      "341010"
--      λ> decimalAfactoradico 2022
--      "2441000"
--      λ> decimalAfactoradico 36288000
--      "A0000000000"
--      λ> map decimalAfactoradico [1..10]
--      ["10","100","110","200","210","1000","1010","1100","1110","1200"]
--      λ> decimalAfactoradico 37199332678990121746799944815083519999999
--      "3KXWVUTSRQPONMLKJIHGFEDCBA9876543210"
--
-- Comprobar con QuickCheck que, para cualquier entero positivo n,
--    factoradicoAdecimal (decimalAfactoradico n) == n
-- ---------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Sistema_factoradico_de_numeracion where

import Data.List (genericIndex, genericLength)
import qualified Data.Map as M ((!), Map, fromList)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª definición de factoradicoAdecimal
-- ====================================

factoradicoAdecimal1 :: String -> Integer
factoradicoAdecimal1 cs = sum (zipWith (*) xs ys)
  where xs = map caracterAentero cs
        n  = length cs
        ys = reverse (take n facts)

-- (caracterAentero c) es la posición del carácter c en la lista de
-- caracteres ['0', '1',..., '9', 'A', 'B',..., 'Z']. Por ejemplo,
--    caracterAentero '0'  ==  0
--    caracterAentero '1'  ==  1
--    caracterAentero '9'  ==  9
--    caracterAentero 'A'  ==  10
--    caracterAentero 'B'  ==  11
--    caracterAentero 'Z'  ==  35
caracterAentero :: Char -> Integer
caracterAentero c =
  head [n | (n,x) <- zip [0..] caracteres, x == c]

-- caracteres es la lista de caracteres
-- ['0', '1',..., '9', 'A', 'B',..., 'Z']
caracteres :: String
caracteres = ['0'..'9'] ++ ['A'..'Z']

-- facts es la lista de los factoriales. Por ejemplo,
--    λ> take 12 facts
--    [1,1,2,6,24,120,720,5040,40320,362880,3628800,39916800]
facts :: [Integer]
facts = scanl (*) 1 [1..]

-- 2ª definición de factoradicoAdecimal
-- ====================================

factoradicoAdecimal2 :: String -> Integer
factoradicoAdecimal2 cs = sum (zipWith (*) xs ys)
    where xs = map caracterAentero2 cs
          n  = length cs
          ys = reverse (take n facts)

-- (caracterAentero2 c) es la posición del carácter c en la lista de
-- caracteres ['0', '1',..., '9', 'A', 'B',..., 'Z']. Por ejemplo,
--    caracterAentero2 '0'  ==  0
--    caracterAentero2 '1'  ==  1
--    caracterAentero2 '9'  ==  9
--    caracterAentero2 'A'  ==  10
--    caracterAentero2 'B'  ==  11
--    caracterAentero2 'Z'  ==  35
caracterAentero2 :: Char -> Integer
caracterAentero2 c = caracteresEnteros M.! c

-- caracteresEnteros es el diccionario cuyas claves son los caracteres y
-- las claves son los números de 0 a 35.
caracteresEnteros :: M.Map Char Integer
caracteresEnteros = M.fromList (zip (['0'..'9'] ++ ['A'..'Z']) [0..])

-- 3ª definición de factoradicoAdecimal
-- ====================================

factoradicoAdecimal3 :: String -> Integer
factoradicoAdecimal3 cs =
  sum (zipWith (*) facts (reverse (map caracterAentero3 cs)))

-- (caracterAentero3 c) es la posición del carácter c en la lista de
-- caracteres ['0', '1',..., '9', 'A', 'B',..., 'Z']. Por ejemplo,
--    caracterAentero3 '0'  ==  0
--    caracterAentero3 '1'  ==  1
--    caracterAentero3 '9'  ==  9
--    caracterAentero3 'A'  ==  10
--    caracterAentero3 'B'  ==  11
--    caracterAentero3 'Z'  ==  35
caracterAentero3 :: Char -> Integer
caracterAentero3 c =
  genericLength (takeWhile (/= c) caracteres)

-- 4ª definición de factoradicoAdecimal
-- ====================================

factoradicoAdecimal4 :: String -> Integer
factoradicoAdecimal4 =
  sum . zipWith (*) facts . reverse . map caracterAentero4

-- (caracterAentero4 c) es la posición del carácter c en la lista de
-- caracteres ['0', '1',..., '9', 'A', 'B',..., 'Z']. Por ejemplo,
--    caracterAentero4 '0'  ==  0
--    caracterAentero4 '1'  ==  1
--    caracterAentero4 '9'  ==  9
--    caracterAentero4 'A'  ==  10
--    caracterAentero4 'B'  ==  11
--    caracterAentero4 'Z'  ==  35
caracterAentero4 :: Char -> Integer
caracterAentero4 =
  genericLength . flip takeWhile caracteres . (/=)

-- 1ª definición de decimalAfactoradico
-- ====================================

decimalAfactoradico1 :: Integer -> String
decimalAfactoradico1 n = aux n (reverse (takeWhile (<=n) facts))
  where aux 0 xs     = ['0' | _ <- xs]
        aux m (x:xs) = enteroAcaracter (m `div` x) : aux (m `mod` x) xs

-- (enteroAcaracter k) es el k-ésimo elemento de la lista
-- ['0', '1',..., '9', 'A', 'B',..., 'Z']. . Por ejemplo,
--    enteroAcaracter 0   ==  '0'
--    enteroAcaracter 1   ==  '1'
--    enteroAcaracter 9   ==  '9'
--    enteroAcaracter 10  ==  'A'
--    enteroAcaracter 11  ==  'B'
--    enteroAcaracter 35  ==  'Z'
enteroAcaracter :: Integer -> Char
enteroAcaracter k = caracteres `genericIndex` k

-- 2ª definición de decimalAfactoradico
-- ====================================

decimalAfactoradico2 :: Integer -> String
decimalAfactoradico2 n = aux n (reverse (takeWhile (<=n) facts))
  where aux 0 xs     = ['0' | _ <- xs]
        aux m (x:xs) = enteroAcaracter2 (m `div` x) : aux (m `mod` x) xs

-- (enteroAcaracter2 k) es el k-ésimo elemento de la lista
-- ['0', '1',..., '9', 'A', 'B',..., 'Z']. . Por ejemplo,
--    enteroAcaracter2 0   ==  '0'
--    enteroAcaracter2 1   ==  '1'
--    enteroAcaracter2 9   ==  '9'
--    enteroAcaracter2 10  ==  'A'
--    enteroAcaracter2 11  ==  'B'
--    enteroAcaracter2 35  ==  'Z'
enteroAcaracter2 :: Integer -> Char
enteroAcaracter2 k = enterosCaracteres M.! k

-- enterosCaracteres es el diccionario cuyas claves son los número de 0
-- a 35 y las claves son los caracteres.
enterosCaracteres :: M.Map Integer Char
enterosCaracteres = M.fromList (zip [0..] caracteres)

-- 3ª definición de decimalAfactoradico
-- ====================================

decimalAfactoradico3 :: Integer -> String
decimalAfactoradico3 n = aux "" 2 (n, 0)
  where aux s _ (0, 0) = s
        aux s m (d, r) = aux (enteroAcaracter3 r: s) (m + 1) (d `divMod` m)

-- (enteroAcaracter3 k) es el k-ésimo elemento de la lista
-- ['0', '1',..., '9', 'A', 'B',..., 'Z']. . Por ejemplo,
--    enteroAcaracter3 0   ==  '0'
--    enteroAcaracter3 1   ==  '1'
--    enteroAcaracter3 9   ==  '9'
--    enteroAcaracter3 10  ==  'A'
--    enteroAcaracter3 11  ==  'B'
--    enteroAcaracter3 35  ==  'Z'
enteroAcaracter3 :: Integer -> Char
enteroAcaracter3 n =
  caracteres !! fromInteger n

-- 4ª definición de decimalAfactoradico
-- ====================================

decimalAfactoradico4 :: Integer -> String
decimalAfactoradico4 = f "" 2 . (, 0)
  where f s _ (0, 0) = s
        f s n (d, r) = f (enteroAcaracter4 r: s) (n + 1) (d `divMod` n)

-- (enteroAcaracter4 k) es el k-ésimo elemento de la lista
-- ['0', '1',..., '9', 'A', 'B',..., 'Z']. . Por ejemplo,
--    enteroAcaracter4 0   ==  '0'
--    enteroAcaracter4 1   ==  '1'
--    enteroAcaracter4 9   ==  '9'
--    enteroAcaracter4 10  ==  'A'
--    enteroAcaracter4 11  ==  'B'
--    enteroAcaracter4 35  ==  'Z'
enteroAcaracter4 :: Integer -> Char
enteroAcaracter4 = (caracteres `genericIndex`)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG1 :: (String -> Integer) -> Spec
specG1 factoradicoAdecimal = do
  it "e1" $
    factoradicoAdecimal "341010" `shouldBe` 463
  it "e2" $
    factoradicoAdecimal "2441000" `shouldBe` 2022
  it "e3" $
    factoradicoAdecimal "A0000000000" `shouldBe` 36288000

specG2 :: (Integer -> String) -> Spec
specG2 decimalAfactoradico = do
  it "e1" $
    decimalAfactoradico 463 `shouldBe` "341010"
  it "e2" $
    decimalAfactoradico 2022 `shouldBe` "2441000"
  it "e3" $
    decimalAfactoradico 36288000 `shouldBe` "A0000000000"

spec :: Spec
spec = do
  describe "def. 1" $ specG1 factoradicoAdecimal1
  describe "def. 2" $ specG1 factoradicoAdecimal2
  describe "def. 3" $ specG1 factoradicoAdecimal3
  describe "def. 4" $ specG1 factoradicoAdecimal4
  describe "def. 1" $ specG2 decimalAfactoradico1
  describe "def. 2" $ specG2 decimalAfactoradico2
  describe "def. 3" $ specG2 decimalAfactoradico3
  describe "def. 4" $ specG2 decimalAfactoradico4

-- La verificación es
--    λ> verifica
--
--    24 examples, 0 failures

-- Propiedad de inverso
-- ====================

prop_factoradico :: Integer -> Property
prop_factoradico n =
  n >= 0 ==>
  factoradicoAdecimal1 (decimalAfactoradico1 n) == n &&
  factoradicoAdecimal2 (decimalAfactoradico2 n) == n &&
  factoradicoAdecimal3 (decimalAfactoradico3 n) == n &&
  factoradicoAdecimal4 (decimalAfactoradico4 n) == n

-- La comprobación es
--    λ> quickCheck prop_factoradico
--    +++ OK, passed 100 tests; 101 discarded.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (decimalAfactoradico1 (10^300000))
--    68191
--    (2.46 secs, 9,088,634,744 bytes)
--    λ> length (decimalAfactoradico2 (10^300000))
--    68191
--    (2.36 secs, 9,088,634,800 bytes)
--    λ> length (decimalAfactoradico3 (10^300000))
--    68191
--    (2.18 secs, 4,490,856,416 bytes)
--    λ> length (decimalAfactoradico4 (10^300000))
--    68191
--    (1.98 secs, 4,490,311,536 bytes)
--
--    λ> length (show (factoradicoAdecimal1 (show (10^50000))))
--    213237
--    (0.93 secs, 2,654,156,680 bytes)
--    λ> length (show (factoradicoAdecimal2 (show (10^50000))))
--    213237
--    (0.51 secs, 2,633,367,168 bytes)
--    λ> length (show (factoradicoAdecimal3 (show (10^50000))))
--    213237
--    (0.93 secs, 2,635,792,192 bytes)
--    λ> length (show (factoradicoAdecimal4 (show (10^50000))))
--    213237
--    (0.43 secs, 2,636,996,848 bytes)
