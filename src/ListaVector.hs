-- ListaVector.hs
-- Transformación de listas a vectores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-diciembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El tipo de los vectores, usando la librería Data.Array, se pueden
-- definir por
--    type Vector a = Array Int a
--
-- Definir la función
--    listaVector :: Num a => [a] -> Vector a
-- tal que (listaVector xs) es el vector correspondiente a la lista
-- xs. Por ejemplo,
--    λ> listaVector [4,7,5]
--    array (1,3) [(1,4),(2,7),(3,5)]
-- ---------------------------------------------------------------------

module ListaVector where

import Data.Array
import Test.Hspec (Spec, hspec, it, shouldBe)

type Vector a = Array Int a

-- 1ª solución
-- ===========

listaVector :: Num a => [a] -> Vector a
listaVector xs = listArray (1,n) xs
  where n = length xs

-- 2ª solución
-- ===========

listaVector2 :: Num a => [a] -> Vector a
listaVector2 xs = array (1,n) (zip [1..n] xs)
  where n = length xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "ej1" $
    show (listaVector [4::Int,7,5]) `shouldBe` "array (1,3) [(1,4),(2,7),(3,5)]"
  it "ej2" $
    show (listaVector2 [4::Int,7,5]) `shouldBe` "array (1,3) [(1,4),(2,7),(3,5)]"

-- La verificación es
--    λ> verifica
--
--    2 examples, 0 failures
