-- Extension_de_un_fichero.hs
-- Extensión de un fichero.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-Noviembre-2014 (actualizado 31-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La extensión de un fichero es la palabra a continuación del último
-- punto en el nombre del fichero. Por ejemplo, la extensión de
-- "documento.txt" es "txt"
--
-- Definir la función
--    extension :: String -> String
-- tal que (extension cs) es la extensión del fichero cs. Por ejemplo,
--    extension "ejercicio.hs"       ==  "hs"
--    extension "documento.txt"      ==  "txt"
--    extension "documento.txt.pdf"  ==  "pdf"
--    extension "resumen"            ==  ""
-- ---------------------------------------------------------------------

module Extension_de_un_fichero where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

extension1 :: String -> String
extension1 cs | '.' `elem` cs = reverse (aux (reverse cs))
              | otherwise     = ""
  where aux []      = []
        aux ('.':_) = []
        aux (d:ds)  = d : aux ds

-- 2ª solución
-- ===========

extension2 :: String -> String
extension2 cs
  | '.' `notElem` cs = ""
  | otherwise        = reverse (takeWhile (/= '.') (reverse cs))

-- 3ª solución
-- ===========

extension3 :: String -> String
extension3 cs
  | '.' `notElem` cs = ""
  | otherwise        = drop n cs
  where n = last [y | ('.',y) <- zip cs [1..]]

-- 4ª solución
-- ===========

extension4 :: String -> String
extension4 cs =
  case break (== '.') (reverse cs) of
    (_, [])     -> ""
    (ds, _) -> reverse ds

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> String) -> Spec
specG extension = do
  it "e1" $
    extension "ejercicio.hs"      `shouldBe` "hs"
  it "e2" $
    extension "documento.txt"     `shouldBe` "txt"
  it "e3" $
    extension "documento.txt.pdf" `shouldBe` "pdf"
  it "e4" $
    extension "resumen"           `shouldBe` ""

spec :: Spec
spec = do
  describe "def. 1" $ specG extension1
  describe "def. 2" $ specG extension2
  describe "def. 3" $ specG extension3
  describe "def. 4" $ specG extension4

-- La verificación es
--    λ> verifica
--    16 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Generador de cadenas. Por ejemplo,
--    λ> generate genCadena
--    "e63.bd8"
--    λ> generate genCadena
--    "yifm5fp9.ywy"
genCadena :: Gen String
genCadena =
  frequency [
    (9, cadenaConPunto),
    (1, arbitrary)
  ]
  where
    cadenaConPunto = do
      xs <- listOf (elements (['a'..'z'] ++ ['0'..'9']))
      ys <-  vectorOf 3 (elements (['a'..'z'] ++ ['0'..'9']))
      return (xs ++ "." ++ ys)

-- La propiedad es
prop_extension :: Property
prop_extension =
  forAll genCadena $ \cs ->
    all (== extension1 cs)
        [extension2 cs,
         extension3 cs,
         extension4 cs]

-- La comprobación es
--    λ> quickCheck prop_extension
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> extension1 (replicate (10^7) 'a' ++ ".txt")
--    "txt"
--    (1.28 secs, 1,360,601,160 bytes)
--    λ> extension2 (replicate (10^7) 'a' ++ ".txt")
--    "txt"
--    (1.30 secs, 1,360,601,040 bytes)
--    λ> extension3 (replicate (10^7) 'a' ++ ".txt")
--    "txt"
--    (1.76 secs, 2,640,601,512 bytes)
--    λ> extension4 (replicate (10^7) 'a' ++ ".txt")
--    "txt"
--    (1.40 secs, 1,360,601,128 bytes)
