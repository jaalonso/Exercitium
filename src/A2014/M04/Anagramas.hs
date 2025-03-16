-- Anagramas.hs
-- Anagramas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-abril-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una palabra es una anagrama de otra si se puede obtener permutando
-- sus letras. Por ejemplo, mora y roma son anagramas de amor.
--
-- Definir la función
--    anagramas :: String -> [String] -> [String]
-- tal que (anagramas x ys) es la lista de los elementos de ys que son
-- anagramas de x. Por ejemplo,
--    λ> anagramas "amor" ["Roma","mola","loma","moRa", "rama"]
--    ["Roma","moRa"]
--    λ> anagramas "rama" ["aMar","amaRa","roMa","marr","aRma"]
--    ["aMar","aRma"]
-- ---------------------------------------------------------------------

module A2014.M04.Anagramas where

import Data.List (sort)
import Data.Char (toLower)
import Data.Function (on)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

anagramas1 :: String -> [String] -> [String]
anagramas1 _ [] = []
anagramas1 x (y:ys) | sonAnagramas1 x y = y : anagramas1 x ys
                    | otherwise         = anagramas1 x ys

-- (sonAnagramas1 xs ys) se verifica si xs e ys son anagramas. Por
-- ejemplo,
--    sonAnagramas1 "amor" "Roma"  ==  True
--    sonAnagramas1 "amor" "mola"  ==  False
sonAnagramas1 :: String -> String -> Bool
sonAnagramas1 xs ys =
  sort (map toLower xs) == sort (map toLower ys)

-- 2ª solución
-- ===========

anagramas2 :: String -> [String] -> [String]
anagramas2 _ [] = []
anagramas2 x (y:ys) | sonAnagramas2 x y = y : anagramas2 x ys
                    | otherwise         = anagramas2 x ys

sonAnagramas2 :: String -> String -> Bool
sonAnagramas2 xs ys =
  (sort . map toLower) xs == (sort . map toLower) ys

-- 3ª solución
-- ===========

anagramas3 :: String -> [String] -> [String]
anagramas3 _ [] = []
anagramas3 x (y:ys) | sonAnagramas3 x y = y : anagramas3 x ys
                    | otherwise         = anagramas3 x ys

sonAnagramas3 :: String -> String -> Bool
sonAnagramas3 = (==) `on` (sort . map toLower)

-- Nota. En la solución anterior se usa la función on ya que
--    (f `on` g) x y
-- es equivalente a
--    f (g x) (g y)
-- Por ejemplo,
--    λ> ((*) `on` (+2)) 3 4
--    30

-- 4ª solución
-- ===========

anagramas4 :: String -> [String] -> [String]
anagramas4 x ys = [y | y <- ys, sonAnagramas1 x y]

-- 5ª solución
-- ===========

anagramas5 :: String -> [String] -> [String]
anagramas5 x = filter (`sonAnagramas1` x)

-- 6ª solución
-- ===========

anagramas6 :: String -> [String] -> [String]
anagramas6 x = filter (((==) `on` (sort . map toLower)) x)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> [String] -> [String]) -> Spec
specG anagramas = do
  it "e1" $
    anagramas "amor" ["Roma","mola","loma","moRa", "rama"] `shouldBe`
      ["Roma","moRa"]
  it "e2" $
    anagramas "rama" ["aMar","amaRa","roMa","marr","aRma"] `shouldBe`
      ["aMar","aRma"]

spec :: Spec
spec = do
  describe "def. 1" $ specG anagramas1
  describe "def. 2" $ specG anagramas2
  describe "def. 3" $ specG anagramas3
  describe "def. 4" $ specG anagramas4
  describe "def. 5" $ specG anagramas5
  describe "def. 6" $ specG anagramas6

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures
