-- Codigo_Morse.hs
-- Código Morse.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-Julio-2014 (actualizado 14-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ------------------------------------------------------------------------
-- El código Morse es un sistema de representación de letras y números
-- mediante señales emitidas de forma intermitente.
--
-- A los signos (letras mayúsculas o dígitos) se le asigna un código
-- como se muestra a continuación
--    |---+-------|---+-------|---+-------|---+-------|
--    | A | .-    | J | .---  | S | ...   | 1 | ..--- |
--    | B | -...  | K | -.-   | T | -     | 2 | ...-- |
--    | C | -.-.  | L | .-..  | U | ..-   | 3 | ....- |
--    | D | -..   | M | --    | V | ...-  | 4 | ..... |
--    | E | .     | N | -.    | W | .--   | 5 | -.... |
--    | F | ..-.  | O | ---   | X | -..-  | 6 | --... |
--    | G | --.   | P | .--.  | Y | -.--  | 7 | ---.. |
--    | H | ....  | Q | --.-  | Z | --..  | 8 | ----. |
--    | I | ..    | R | .-.   | 0 | .---- | 9 | ----- |
--    |---+-------|---+-------|---+-------|---+-------|
--
-- El código Morse de las palabras se obtiene a partir del de sus
-- caracteres insertando un espacio entre cada uno. Por ejemplo, el
-- código de "todo" es "- --- -.. ---"
--
-- El código Morse de las frase se obtiene a partir del de sus
-- palabras insertando un espacio entre cada uno. Por ejemplo, el
-- código de "todo o nada" es "- --- -.. ---  ---  -. .- -.. .-"
--
-- Definir las funciones
--    fraseAmorse :: String -> String
--    morseAfrase :: String -> String
-- tales que
-- + (fraseAmorse cs) es la traducción de la frase cs a Morse. Por
--   ejemplo,
--      λ> fraseAmorse "En todo la medida"
--      ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
-- + (morseAfrase cs) es la frase cuya traducción a Morse es cs. Por
--   ejemplo,
--      λ> morseAfrase ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
--      "EN TODO LA MEDIDA"
-- ------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Codigo_Morse where

import Data.Char (toUpper, isAlphaNum)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

-- caracteres es la lista ordenada de las caracteres (letras mayúsculas
-- y dígitos) que se usan en los mensajes Morse.
caracteres :: [Char]
caracteres = ['A'..'Z'] ++ ['0'..'9']

-- morse es la lista de los códigos Morse correspondientes a la lista
-- de caracteres.
morse :: [String]
morse = [".-","-...","-.-.","-..",".","..-.","--.","....","..",".---",
         "-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-",
         "..-","...-",".--","-..-","-.--","--..",".----","..---","...--",
         "....-",".....","-....","--...","---..","----.","-----"]

-- (correspondiente xs ys x) es el elemento de ys en la misma posición
-- que x en xs. Por ejemplo,
--    correspondiente [1..10] [2,4..20] 3  ==  6
correspondiente :: Ord a => [a] -> [b] -> a -> b
correspondiente xs ys x =
  head [y | (z,y) <- zip xs ys, z == x]

-- (caracterAmorse x) es el código Morse correspondiente al carácter
-- x. Por ejemplo,
--    caracterAmorse 'A'  ==  ".-"
--    caracterAmorse 'B'  ==  "-..."
--    caracterAmorse '1'  ==  "..---"
--    caracterAmorse 'a'  ==  ".-"
caracterAmorse :: Char -> String
caracterAmorse =
  correspondiente caracteres morse . toUpper

-- (morseAcaracter x) es el carácter cuyo código Morse es x. Por
-- ejemplo,
--    morseAcaracter ".-"     ==  'A'
--    morseAcaracter "-..."   ==  'B'
--    morseAcaracter "..---"  ==  '1'
morseAcaracter :: String -> Char
morseAcaracter =
  correspondiente morse caracteres

-- (palabraAmorse cs) es el código Morse correspondiente a la palabra
-- cs. Por ejemplo,
--    palabraAmorse "En"  ==  ". -."
palabraAmorse :: [Char] -> String
palabraAmorse = unwords . map caracterAmorse

-- (morseApalabra cs) es la palabra cuyo traducción a Morse es cs. Por
-- ejemplo,
--    morseApalabra ". -."  ==  "EN"
morseApalabra :: String -> [Char]
morseApalabra = map morseAcaracter . words

-- (fraseAmorse cs) es la traducción de la frase cs a Morse. Por ejemplo,
--    λ> fraseAmorse "En todo la medida"
--    ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
fraseAmorse1 :: String -> String
fraseAmorse1 = intercalate "  " . map palabraAmorse . words

-- Ejemplo de cálculo
--    fraseAmorse "En todo la medida"
--    = (intercalate "  " . map palabraAmorse . words)
--      "En todo la medida"
--    = (intercalate "  " . map palabraAmorse)
--      ["En","todo","la","medida"]
--    = intercalate "  " [". -.","- --- -.. ---",".-.. .-","-- . -.. .. -.. .-"]
--    = ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"

-- (morseAfrase cs) es la frase cuya traducción a Morse es cs. Por
-- ejemplo,
--    λ> morseAfrase ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
--    "EN TODO LA MEDIDA"
morseAfrase1 :: String -> String
morseAfrase1 = unwords . map morseApalabra . splitOn "  "

-- Ejemplo de cálculo
--    morseAfrase ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
--    = (unwords . map morseApalabra)
--      ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
--    = (unwords . map morseApalabra)
--      [". -.","- --- -.. ---",".-.. .-","-- . -.. .. -.. .-"]
--    = unwords ["EN","TODO","LA","MEDIDA"]
--    = "EN TODO LA MEDIDA"

-- 2ª solución
-- ===========

-- Diccionario de Morse
diccionarioMorse :: [(Char, String)]
diccionarioMorse =
  zip (['A'..'Z'] ++ ['0'..'9'])
      [".-","-...","-.-.","-..",".","..-.","--.","....","..",".---",
       "-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-",
       "..-","...-",".--","-..-","-.--","--..",".----","..---","...--",
       "....-",".....","-....","--...","---..","----.","-----"]

fraseAmorse2 :: String -> String
fraseAmorse2 =
  intercalate "  "
  . map (intercalate " "
  . map caracterAmorse2)
  . words
  . map toUpper

caracterAmorse2 :: Char -> String
caracterAmorse2 c =
  fromJust (lookup c diccionarioMorse)

morseAfrase2 :: String -> String
morseAfrase2 =
  unwords
  . map (map morseAcaracter2 . words)
  . splitOn "  "

morseAcaracter2 :: String -> Char
morseAcaracter2 m =
  fromJust (lookup m (map (\(a,b) -> (b,a)) diccionarioMorse))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG1 :: (String -> String) -> Spec
specG1 fraseAmorse = do
  it "e1" $
    fraseAmorse "En todo la medida" `shouldBe`
    ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"

specG2 :: (String -> String) -> Spec
specG2 morseAfrase = do
  it "e1" $
    morseAfrase ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
    `shouldBe` "EN TODO LA MEDIDA"

spec :: Spec
spec = do
  describe "def. 1" $ specG1 fraseAmorse1
  describe "def. 2" $ specG1 fraseAmorse2
  describe "def. 1" $ specG2 morseAfrase1
  describe "def. 2" $ specG2 morseAfrase2

-- La verificación es
--    λ> verifica
--    21 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Property
prop_equivalencia =
  forAll (listOf (elements (['A'..'Z'] ++ ['0'..'9'] ++ " "))) $ \xs ->
  let ys = fraseAmorse1 xs in
  fraseAmorse2 xs == ys && morseAfrase1 ys == morseAfrase2 ys

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (fraseAmorse1 (take (3*10^6) (cycle "ABC ")))
--    10499998
--    (2.03 secs, 3,318,602,592 bytes)
--    λ> length (fraseAmorse2 (take (3*10^6) (cycle "ABC ")))
--    10499998
--    (0.93 secs, 2,694,602,584 bytes)
--
--    λ> length (morseAfrase1 ejemplo)
--    2999999
--    (4.04 secs, 6,606,601,512 bytes)
--    λ> length (morseAfrase2 ejemplo)
--    2999999
--    (0.79 secs, 3,288,600,112 bytes)
