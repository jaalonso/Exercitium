-- Longitud_SCM.hs
-- Longitud de la subsecuencia común máxima.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-septiembre-2023
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
-- Se desea encontrar la longitud de las subsecuencias comunes más
-- largas de dos secuencias de caracteres dadas.
--
-- Definir la función
--    longitudSCM :: Eq a => [a] -> [a] -> Int
-- tal que (longitudSCM xs ys) es la longitud de la subsecuencia máxima
-- de xs e ys. Por ejemplo,
--    longitudSCM "amapola" "matamoscas" == 4
--    longitudSCM "atamos" "matamoscas"  == 6
--    longitudSCM "aaa" "bbbb"           == 0
-- ---------------------------------------------------------------------

module Longitud_SCM where

import Data.Array (Array,(!), array, listArray)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definición (por recursión)
-- =============================

longitudSCM1 :: Eq a => [a] -> [a] -> Int
longitudSCM1 [] _ = 0
longitudSCM1 _ [] = 0
longitudSCM1 (x:xs) (y:ys)
  | x == y    = 1 + longitudSCM1 xs ys
  | otherwise = max (longitudSCM1 (x:xs) ys) (longitudSCM1 xs (y:ys))

-- 2ª definición (con programación dinámica)
-- =========================================

longitudSCM2 :: Eq a => [a] -> [a] -> Int
longitudSCM2 xs ys = matrizLongitudSCM2 xs ys ! (n,m)
  where n = length xs
        m = length ys

-- (matrizLongitudSCM2 xs ys) es la matriz de orden (n+1)x(m+1) (donde n
-- y m son los números de elementos de xs e ys, respectivamente) tal que
-- el valor en la posición (i,j) es la longitud de la SCM de los i
-- primeros elementos de xs y los j primeros elementos de ys. Por ejemplo,
--    λ> elems (matrizLongitudSCM2 "amapola" "matamoscas")
--    [0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,2,2,2,2,2,2,
--     0,1,2,2,2,2,2,2,2,3,3,0,1,2,2,2,2,2,2,2,3,3,0,1,2,2,2,2,3,3,3,3,3,
--     0,1,2,2,2,2,3,3,3,3,3,0,1,2,2,3,3,3,3,3,4,4]
-- Gráficamente,
--       m a t a m o s c a s
--    [0,0,0,0,0,0,0,0,0,0,0,
-- a   0,0,1,1,1,1,1,1,1,1,1,
-- m   0,1,1,1,1,2,2,2,2,2,2,
-- a   0,1,2,2,2,2,2,2,2,3,3,
-- p   0,1,2,2,2,2,2,2,2,3,3,
-- o   0,1,2,2,2,2,3,3,3,3,3,
-- l   0,1,2,2,2,2,3,3,3,3,3,
-- a   0,1,2,2,3,3,3,3,3,4,4]
matrizLongitudSCM2 :: Eq a => [a] -> [a] -> Array (Int,Int) Int
matrizLongitudSCM2 xs ys = q
  where
    n = length xs
    m = length ys
    v = listArray (1,n) xs
    w = listArray (1,m) ys
    q = array ((0,0),(n,m)) [((i,j), f i j) | i <- [0..n], j <- [0..m]]
      where f 0 _ = 0
            f _ 0 = 0
            f i j | v ! i == w ! j = 1 + q ! (i-1,j-1)
                  | otherwise      = max (q ! (i-1,j)) (q ! (i,j-1))

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> longitudSCM1 (take 18 (cycle [1,3])) (take 18 (cycle [2,3]))
--    9
--    (13.90 secs, 8,883,660,048 bytes)
--    λ> longitudSCM2 (take 18 (cycle [1,3])) (take 18 (cycle [2,3]))
--    9
--    (0.01 secs, 953,880 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    longitudSCM1 "amapola" "matamoscas" `shouldBe` 4
  it "e2" $
    longitudSCM1 "atamos" "matamoscas"  `shouldBe` 6
  it "e3" $
    longitudSCM1 "aaa" "bbbb"           `shouldBe` 0
  it "e4" $
    longitudSCM2 "amapola" "matamoscas" `shouldBe` 4
  it "e5" $
    longitudSCM2 "atamos" "matamoscas"  `shouldBe` 6
  it "e6" $
    longitudSCM2 "aaa" "bbbb"           `shouldBe` 0

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
--    Finished in 0.0025 seconds
--    6 examples, 0 failures
