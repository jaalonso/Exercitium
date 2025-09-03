-- Entero_positivo_de_la_cadena.hs
-- Entero positivo de la cadena.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-Junio-2014 (actualizado 3-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    enteroPositivo :: String -> Maybe Integer
-- tal que (enteroPositivo cs) es justo el contenido de la cadena cs, si
-- dicho contenido es un entero positivo, y Nothing en caso contrario.
-- Por ejemplo,
--    enteroPositivo "235"    ==  Just 235
--    enteroPositivo "-235"   ==  Nothing
--    enteroPositivo "23.5"   ==  Nothing
--    enteroPositivo "235 "   ==  Nothing
--    enteroPositivo "cinco"  ==  Nothing
--    enteroPositivo ""       ==  Nothing
-- ---------------------------------------------------------------------

module Entero_positivo_de_la_cadena where

import Data.Maybe (listToMaybe)
import Numeric (readDec)
import Data.Char (isDigit)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

enteroPositivo1 :: String -> Maybe Integer
enteroPositivo1 ""                   = Nothing
enteroPositivo1 cs | todosDigitos1 cs = Just (read cs)
                   | otherwise        = Nothing

-- (todosDigitos cs) se verifica si todos los elementos de cs son
-- dígitos. Por ejemplo,
--    todosDigitos "235"    ==  True
--    todosDigitos "-235"   ==  False
--    todosDigitos "23.5"   ==  False
--    todosDigitos "235 "   ==  False
--    todosDigitos "cinco"  ==  False
todosDigitos1 :: String -> Bool
todosDigitos1 cs =
  and [esDigito1 c | c <- cs]

-- (esDigito c) se verifica si el carácter c es un dígito. Por ejemplo,
--    esDigito '5'  ==  True
--    esDigito 'a'  ==  False
esDigito1 :: Char -> Bool
esDigito1 c = c `elem` "0123456789"

-- 2ª solución
-- ===========

enteroPositivo2 :: String -> Maybe Integer
enteroPositivo2 ""                   = Nothing
enteroPositivo2 cs | todosDigitos2 cs = Just (read cs)
                   | otherwise        = Nothing

todosDigitos2 :: String -> Bool
todosDigitos2 cs =
  and [esDigito2 c | c <- cs]

esDigito2 :: Char -> Bool
esDigito2 c = c `elem` ['0'..'9']

-- 3ª solución
-- ===========

enteroPositivo3 :: String -> Maybe Integer
enteroPositivo3 ""                   = Nothing
enteroPositivo3 cs | todosDigitos3 cs = Just (read cs)
                   | otherwise        = Nothing

todosDigitos3 :: String -> Bool
todosDigitos3 cs =
  and [esDigito3 c | c <- cs]

esDigito3 :: Char -> Bool
esDigito3 = (`elem` ['0'..'9'])

-- 4ª solución
-- ===========

enteroPositivo4 :: String -> Maybe Integer
enteroPositivo4 ""                   = Nothing
enteroPositivo4 cs | todosDigitos4 cs = Just (read cs)
                   | otherwise        = Nothing

todosDigitos4 :: String -> Bool
todosDigitos4 cs =
  and [esDigito4 c | c <- cs]

esDigito4 :: Char -> Bool
esDigito4 c = '0' <= c && c <= '9'

-- 5ª solución
-- ===========

enteroPositivo5 :: String -> Maybe Integer
enteroPositivo5 ""                   = Nothing
enteroPositivo5 cs | todosDigitos5 cs = Just (read cs)
                   | otherwise        = Nothing

todosDigitos5 :: String -> Bool
todosDigitos5 cs =
  and [isDigit c | c <- cs]

-- 6ª solución
-- ===========

enteroPositivo6 :: String -> Maybe Integer
enteroPositivo6 ""                   = Nothing
enteroPositivo6 cs | todosDigitos6 cs = Just (read cs)
                   | otherwise        = Nothing

todosDigitos6 :: String -> Bool
todosDigitos6 []     = True
todosDigitos6 (c:cs) = isDigit c && todosDigitos6 cs

-- 7ª solución
-- ===========

enteroPositivo7 :: String -> Maybe Integer
enteroPositivo7 ""                   = Nothing
enteroPositivo7 cs | todosDigitos7 cs = Just (read cs)
                   | otherwise        = Nothing

todosDigitos7 :: String -> Bool
todosDigitos7 = foldr ((&&) . isDigit) True

-- 8ª solución
-- ===========

enteroPositivo8 :: String -> Maybe Integer
enteroPositivo8 ""                   = Nothing
enteroPositivo8 cs | todosDigitos8 cs = Just (read cs)
                   | otherwise        = Nothing

todosDigitos8 :: String -> Bool
todosDigitos8 = all isDigit

-- 9ª solución
-- ===========

enteroPositivo9 :: String -> Maybe Integer
enteroPositivo9 cs
  | null xs   = Nothing
  | otherwise = Just (head xs)
  where xs = [x | (x,y) <- readDec cs, null y]

-- Nota. En la solución anterior se ha usado la función readDec de la
-- librería Numeric. El valor de (readDec cs) es la lista de los pares
-- (x,y) tales que x es el entero positivo al principio de cs e y es el
-- resto. Por ejemplo,
--    readDec "235"    ==  [(235,"")]
--    readDec "-235"   ==  []
--    readDec "23.5"   ==  [(23,".5")]
--    readDec "235 "   ==  [(235," ")]
--    readDec "cinco"  ==  []
--    readDec ""       ==  []

-- 10ª solución
-- ===========

enteroPositivo10 :: String -> Maybe Integer
enteroPositivo10 =
    fmap fst . listToMaybe . filter (null . snd) . readDec

-- Nota. En la solución anterior se ha usado la función listToMaybe
-- (de la librería Data.Maybe) tal que (listToMaybe xs) es Nothing si xs
-- es la lista vacía o (Just x) donde x es el primer elemento de xs. Por
-- ejemplo,
--    listToMaybe []       ==  Nothing
--    listToMaybe [3,2,5]  ==  Just 3
-- y la función fmap tal que (fmap f x) le aplica la función f a los
-- elementos de x. Por ejemplo,
--    fmap (+2) (Just 3)  ==  Just 5
--    fmap (+2) Nothing   ==  Nothing
--    fmap (+2) [3,4,6]   ==  [5,6,8]
--    fmap (+2) []        ==  []

-- Nota. Ejemplos de cálculo con enteroPositivo3
--    enteroPositivo10 "325"
--    = (fmap fst . listToMaybe . filter (null . snd) . readDec) "325"
--    = (fmap fst . listToMaybe . filter (null . snd)) [(325,"")]
--    = (fmap fst . listToMaybe) [(325,"")]
--    = fmap fst (Just (325,""))
--    = Just 325
--
--    enteroPositivo10 "32.5"
--    = (fmap fst . listToMaybe . filter (null . snd) . readDec) "32.5"
--    = (fmap fst . listToMaybe . filter (null . snd)) [(32,".5")]
--    = (fmap fst . listToMaybe ) []
--    = fmap fst Nothing
--    = Nothing

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> Maybe Integer) -> Spec
specG enteroPositivo = do
  it "e1" $
    enteroPositivo "235"    `shouldBe`  Just 235
  it "e2" $
    enteroPositivo "-235"   `shouldBe`  Nothing
  it "e3" $
    enteroPositivo "23.5"   `shouldBe`  Nothing
  it "e4" $
    enteroPositivo "235 "   `shouldBe`  Nothing
  it "e5" $
    enteroPositivo "cinco"  `shouldBe`  Nothing
  it "e6" $
    enteroPositivo ""       `shouldBe`  Nothing

spec :: Spec
spec = do
  describe "def. 1"  $ specG enteroPositivo1
  describe "def. 2"  $ specG enteroPositivo2
  describe "def. 3"  $ specG enteroPositivo3
  describe "def. 4"  $ specG enteroPositivo4
  describe "def. 5"  $ specG enteroPositivo5
  describe "def. 6"  $ specG enteroPositivo6
  describe "def. 7"  $ specG enteroPositivo7
  describe "def. 8"  $ specG enteroPositivo8
  describe "def. 9"  $ specG enteroPositivo9
  describe "def. 10" $ specG enteroPositivo10

-- La verificación es
--    λ> verifica
--    60 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- Generador de cadenas. Por ejemplo.
--    λ> generate cadenaArbitraria
--    "69883777219"
--    λ> generate cadenaArbitraria
--    "iyodnfsw2m78mhu651bvtt7"
cadenaArbitraria :: Gen String
cadenaArbitraria = frequency [
  (1, listOf (elements ['0'..'9'])),                -- 50% solo dígitos
  (1, listOf (elements (['0'..'9'] ++ ['a'..'z']))) -- 50% alfanuméricos
  ]

-- La propiedad es
prop_enteroPositivo :: Property
prop_enteroPositivo = forAll cadenaArbitraria $ \s ->
  all (== enteroPositivo1 s)
      [enteroPositivo2 s,
       enteroPositivo3 s,
       enteroPositivo4 s,
       enteroPositivo5 s,
       enteroPositivo6 s,
       enteroPositivo7 s,
       enteroPositivo8 s,
       enteroPositivo9 s,
       enteroPositivo10 s]

-- La verificación es
--    λ> quickCheck prop_enteroPositivo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ej = replicate (2*10^6) '5'
--    λ> (length . show) <$> enteroPositivo1 ej
--    Just 2000000
--    (2.19 secs, 2,187,674,312 bytes)
--    λ> (length . show) <$> enteroPositivo2 ej
--    Just 2000000
--    (1.97 secs, 2,235,673,400 bytes)
--    λ> (length . show) <$> enteroPositivo3 ej
--    Just 2000000
--    (1.52 secs, 1,243,673,336 bytes)
--    λ> (length . show) <$> enteroPositivo4 ej
--    Just 2000000
--    (1.87 secs, 1,467,673,352 bytes)
--    λ> (length . show) <$> enteroPositivo5 ej
--    Just 2000000
--    (1.45 secs, 1,243,673,400 bytes)
--    λ> (length . show) <$> enteroPositivo6 ej
--    Just 2000000
--    (1.49 secs, 1,099,673,184 bytes)
--    λ> (length . show) <$> enteroPositivo7 ej
--    Just 2000000
--    (1.04 secs, 1,035,673,176 bytes)
--    λ> (length . show) <$> enteroPositivo8 ej
--    Just 2000000
--    (0.97 secs, 1,019,673,232 bytes)
--    λ> (length . show) <$> enteroPositivo9 ej
--    Just Interrupted.
--    λ> (length . show) <$> enteroPositivo10 ej
--    Just Interrupted.
