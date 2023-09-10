-- Subsecuencia_comun_maxima.hs
-- Subsecuencia común_máxima
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-septiembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Si a una secuencia X de elementos (pongamos por ejemplo, caracteres)
-- le quitamos algunos de ellos y dejamos los que quedan en el orden en
-- el que aparecían originalmente tenemos lo que se llama una
-- subsecuencia de X. Por ejemplo, "aaoa" es una subsecuencia de la
-- secuencia "amapola".
--
-- El término también se aplica cuando quitamos todos los elementos (es
-- decir, la secuencia vacía es siempre subsecuencia de cualquier
-- secuencia) o cuando no quitamos ninguno (lo que significa que
-- cualquier secuencia es siempre subsecuencia de sí misma).
--
-- Dadas dos secuencias X e Y, decimos que Z es una subsecuencia común
-- de X e Y si Z es subsecuencia de X y de Y. Por ejemplo, si X =
-- "amapola" e Y = "matamoscas", la secuencia "aaoa" es una de las
-- subsecuencias comunes de X e Y más larga, con longitud 4, ya que no
-- hay ninguna subsecuencia común a X e Y de longitud mayor que
-- 4. También son subsecuencias comunes de longitud 4 "maoa" o "amoa".
--
-- Definir la función
--    scm :: Eq a => [a] -> [a] -> [a]
-- tal que (scm xs ys) es una de las subsecuencias comunes de longitud
-- máxima de xs e ys. Por ejemplo,
--    scm "amapola" "matamoscas" == "amoa"
--    scm "atamos" "matamoscas"  == "atamos"
--    scm "aaa" "bbbb"           == ""
-- ---------------------------------------------------------------------

module Subsecuencia_comun_maxima where

import Data.Array (Array, (!), array, listArray)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definición (por recursión)
-- =============================

scm1 :: Eq a => [a] -> [a] -> [a]
scm1 [] _ = []
scm1 _ [] = []
scm1 (x:xs) (y:ys)
  | x == y    = x : scm1 xs ys
  | otherwise = mayor (scm1 (x:xs) ys) (scm1 xs (y:ys))

-- (mayor xs ys) es la cadena más larga de xs e ys.
--    mayor "hola" "buenas"  ==  "buenas"
--    mayor "hola" "pera"    ==  "hola"
mayor :: [a] -> [a] -> [a]
mayor xs ys
  | length xs >= length ys = xs
  | otherwise              = ys

-- 2ª definición (con programación dinámica)
-- =========================================

scm2 :: Eq a => [a] -> [a] -> [a]
scm2 xs ys = reverse (matrizSCM2 xs ys ! (n,m))
  where n = length xs
        m = length ys

-- (matrizSCM2 xs ys) es la matriz de orden (n+1)x(m+1) (donde n
-- y m son los números de elementos de xs e ys, respectivamente) tal que
-- el valor en la posición (i,j) es una SCM de los i primeros
-- elementos de xs y los j primeros elementos de ys. Por ejemplo,
--    λ> elems (matrizSCM2 "amapola" "matamoscas")
--    ["","","","","","","","","","","","","","a","a","a","a","a","a",
--     "a","a","a","","m","a","a","a","ma","ma","ma","ma","ma","ma","",
--     "m","am","am","aa","ma","ma","ma","ma","ama","ama","","m","am",
--     "am","aa","ma","ma","ma","ma","ama","ama","","m","am","am","aa",
--     "ma","oma","oma","oma","ama","ama","","m","am","am","aa","ma",
--     "oma","oma","oma","ama","ama","","m","am","am","aam","aam","oma",
--     "oma","oma","aoma","aoma"]
-- Gráficamente,
--        m   a    t    a     m     o     s     c     a      s
--    ["","" ,""  ,""  ,""   ,""   ,""   ,""   ,""   ,""    ,"",
-- a   "","" ,"a" ,"a" ,"a"  ,"a"  ,"a"  ,"a"  ,"a"  ,"a"   ,"a",
-- m   "","m","a" ,"a" ,"a"  ,"ma" ,"ma" ,"ma" ,"ma" ,"ma"  ,"ma",
-- a   "","m","am","am","aa" ,"ma" ,"ma" ,"ma" ,"ma" ,"ama" ,"ama",
-- p   "","m","am","am","aa" ,"ma" ,"ma" ,"ma" ,"ma" ,"ama" ,"ama",
-- o   "","m","am","am","aa" ,"ma" ,"oma","oma","oma","ama" ,"ama",
-- l   "","m","am","am","aa" ,"ma" ,"oma","oma","oma","ama" ,"ama",
-- a   "","m","am","am","aam","aam","oma","oma","oma","aoma","aoma"]
matrizSCM2 :: Eq a => [a] -> [a] -> Array (Int,Int) [a]
matrizSCM2 xs ys = q where
  q = array ((0,0),(n,m)) [((i,j), f i j) | i <- [0..n], j <- [0..m]]
  n = length xs
  m = length ys
  v = listArray (1,n) xs
  w = listArray (1,m) ys
  f 0 _ = []
  f _ 0 = []
  f i j | v ! i == w ! j = (v!i) : (q ! (i-1,j-1))
        | otherwise      = mayor (q ! (i-1,j)) (q ! (i,j-1))

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (scm1 (take 18 (cycle [1,3])) (take 18 (cycle [2,3])))
--    9
--    (20.17 secs, 11,436,759,992 bytes)
--    λ> length (scm2 (take 18 (cycle [1,3])) (take 18 (cycle [2,3])))
--    9
--    (0.00 secs, 1,013,624 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    scm1 "amapola" "matamoscas" `shouldBe` "amoa"
  it "e2" $
    scm1 "atamos" "matamoscas"  `shouldBe` "atamos"
  it "e3" $
    scm1 "aaa" "bbbb"           `shouldBe` ""
  it "e4" $
    scm2 "amapola" "matamoscas" `shouldBe` "amoa"
  it "e5" $
    scm2 "atamos" "matamoscas"  `shouldBe` "atamos"
  it "e6" $
    scm2 "aaa" "bbbb"           `shouldBe` ""

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--    e6
--
--    Finished in 0.0026 seconds
--    6 examples, 0 failures
