-- Acronimos.hs
-- Acrónimos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-Diciembre-2014 (actualizado 20-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- A partir de una palabra de puede formar un acrónimo uniendo un
-- prefijo de la primera con un sufijo de la segunda. Por ejemplo,
-- + "ofimática"   es un acrónimo de "oficina" e "informática"
-- + "informática" es un acrónimo de "información" y "automática"
-- + "teleñeco"    es un acrónimo de "televisión" y "muñeco"
--
-- Definir la función
--    esAcronimo :: String -> String -> String -> Bool
-- tal que (esAcronimo xs ys zs) se verifica si xs es un acrónimo de ys
-- y zs. Por ejemplo,
--    esAcronimo "ofimatica" "oficina" "informatica"       ==  True
--    esAcronimo "informatica" "informacion" "automatica"  ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Acronimos where

import Data.List (isPrefixOf, isSuffixOf, inits, tails)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

esAcronimo1 :: String -> String -> String -> Bool
esAcronimo1 xs ys zs =
  xs `elem` acronimos ys zs

-- (acronimos xs ys) es la lista de acrónimos de xs e ys. Por ejemplo,
--    λ> acronimos "ab" "cde"
--    ["cde","de","e","","acde","ade","ae","a","abcde","abde","abe","ab"]
acronimos :: String -> String -> [String]
acronimos xs ys =
  [us++vs | us <- inits xs, vs <- tails ys]

-- 2ª solución
-- ===========

esAcronimo2 :: String -> String -> String -> Bool
esAcronimo2 xs ys zs =
  or [p `isPrefixOf` ys && s `isSuffixOf` zs |
      (p, s) <- zip (inits xs) (tails xs)]

-- 3ª solución
-- ===========

esAcronimo3 :: String -> String -> String -> Bool
esAcronimo3 xs ys zs =
  or [isPrefixOf us ys && isSuffixOf vs zs |
      (us,vs) <- [splitAt n xs | n <- [0..length xs]]]

-- 4ª solución
-- ===========

esAcronimo4 :: String -> String -> String -> Bool
esAcronimo4 xs ys zs = any cumpleCondicion particiones
  where
    particiones = [splitAt k xs | k <- [0 .. length xs]]
    cumpleCondicion (p, s) = p `isPrefixOf` ys && s `isSuffixOf` zs

-- 5ª solución
-- ===========

esAcronimo5 :: String -> String -> String -> Bool
esAcronimo5 xs ys zs =
  longPrefijo xs ys + longSufijo xs zs >= length xs

-- (longPrefijo xs ys) es el máximo número de caracteres que coinciden
-- al principio de xs e ys. Por ejemplo,
--    longPrefijo "ofimatica" "oficina" == 3
longPrefijo :: String -> String -> Int
longPrefijo xs ys =
  length (takeWhile id (zipWith (==) xs ys))

-- (longSufijo xs ys) es el máximo número de caracteres que coinciden
-- al final de xs e ys. Por ejemplo,
--    longSufijo "ofimatica" "automatica" == 6
longSufijo :: String -> String -> Int
longSufijo xs ys =
  longPrefijo (reverse xs) (reverse ys)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> String -> String -> Bool) -> Spec
specG esAcronimo = do
  it "e1" $
    esAcronimo "ofimatica" "oficina" "informatica"       `shouldBe`  True
  it "e2" $
    esAcronimo "informatica" "informacion" "automatica"  `shouldBe`  True

spec :: Spec
spec = do
  describe "def. 1" $ specG esAcronimo1
  describe "def. 2" $ specG esAcronimo2
  describe "def. 3" $ specG esAcronimo3
  describe "def. 4" $ specG esAcronimo4
  describe "def. 5" $ specG esAcronimo5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Definimos un tipo de dato para nuestros casos de prueba
data Ejemplo = E String String String
  deriving Show

-- Generador de ejemplos formados por tres cadenas aleatorias. Por ejemplo,
--    λ> generate ejemploAleatorio
--    E "xzkyfgdmnkifjxswflcucsoki" "ij" "hudbauuymqbixlygyqrsbyclbup"
ejemploAleatorio :: Gen Ejemplo
ejemploAleatorio = do
  xs <- listOf (elements ['a'..'z'])
  ys <- listOf (elements ['a'..'z'])
  zs <- listOf (elements ['a'..'z'])
  return (E xs ys zs)

-- Generador de ejemplos formados por tres cadenas tales que la primera
-- es acrónimo de la segunda y la tercera.. Por ejemplo,
--    λ> generate ejemploValido
--    E "dqvjbbfdmglqvjrpwcze" "dqvjbbfdmggu" "sxjxlqvjrpwcze"
ejemploValido :: Gen Ejemplo
ejemploValido = do
  ys <- listOf (elements ['a'..'z'])
  zs <- listOf (elements ['a'..'z'])
  lenP <- choose (0, length ys)
  let p = take lenP ys
  lenS <- choose (0, length zs)
  let s = drop (length zs - lenS) zs
  let xs = p ++ s
  return (E xs ys zs)

-- Ejemplo es una subclase de Arbitrary.
instance Arbitrary Ejemplo where
  arbitrary = frequency
    [ (1, ejemploAleatorio), -- 10% de veces: cadenas totalmente aleatorias
      (9, ejemploValido)     -- 90% de veces: acrónimos construidos válidos
    ]

-- La propiedad es
prop_esAcronimo :: Ejemplo -> Bool
prop_esAcronimo (E xs ys zs) =
  all (== esAcronimo1 xs ys zs)
      [esAcronimo2 xs ys zs,
       esAcronimo3 xs ys zs,
       esAcronimo4 xs ys zs,
       esAcronimo5 xs ys zs]

-- La comprobación es
--    λ> quickCheck prop_esAcronimo
--    +++ OK, passed 100 tests.

-- La propiedad para que indique el porcentaje de ejemplos válidos
-- generados.
prop_esAcronimo2 :: Ejemplo -> Property
prop_esAcronimo2 (E xs ys zs) =
  collect resultado1 $ all (== resultado1)
                           [esAcronimo2 xs ys zs,
                            esAcronimo3 xs ys zs,
                            esAcronimo4 xs ys zs,
                            esAcronimo5 xs ys zs]
  where resultado1 = esAcronimo1 xs ys zs

-- La comprobación es
--    λ> quickCheck prop_esAcronimo2
--    +++ OK, passed 100 tests:
--    88% True
--    12% False
--    (0.06 secs, 39,303,544 bytes)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ej1 = replicate 500 'a'
--    λ> ej2 = replicate 500 'b'
--    λ> ej3 = ej1 ++ ej2
--    λ> esAcronimo1 ej3 ej1 ej2
--    True
--    (1.14 secs, 3,547,840,920 bytes)
--    λ> esAcronimo2 ej3 ej1 ej2
--    True
--    (0.01 secs, 5,918,168 bytes)
--    λ> esAcronimo3 ej3 ej1 ej2
--    True
--    (0.01 secs, 16,846,824 bytes)
--    λ> esAcronimo4 ej3 ej1 ej2
--    True
--    (0.03 secs, 16,810,696 bytes)
--    λ> esAcronimo5 ej3 ej1 ej2
--    True
--    (0.01 secs, 794,488 bytes)
--
--    λ> ej4 = replicate 20000 'a'
--    λ> ej5 = replicate 20000 'b'
--    λ> ej6 = ej4 ++ ej5
--    λ> esAcronimo2 ej6 ej4 ej5
--    True
--    (3.64 secs, 9,576,349,584 bytes)
--    λ> esAcronimo3 ej6 ej4 ej5
--    True
--    (11.29 secs, 25,610,518,784 bytes)
--    λ> esAcronimo4 ej6 ej4 ej5
--    True
--    (11.31 secs, 25,609,078,680 bytes)
--    λ> esAcronimo5 ej6 ej4 ej5
--    True
--    (0.01 secs, 8,438,528 bytes)
