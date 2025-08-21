-- Seleccion_con_fallo.hs
-- Selección hasta el primero que falla inclusive.
-- José A. Alonso <https://jaalonso.github.io>
-- Sevilla, 18-junio-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    seleccionConFallo :: (a -> Bool) -> [a] -> [a]
-- tal que (seleccionConFallo p xs) es la lista de los elementos de xs
-- que cumplen el predicado p hasta el primero que no lo cumple
-- inclusive. Por ejemplo,
--    seleccionConFallo (<5) [3,2,5,7,1,0]  ==  [3,2,5]
--    seleccionConFallo odd [1..4]          ==  [1,2]
--    seleccionConFallo odd [1,3,5]         ==  [1,3,5]
--    seleccionConFallo (<5) [10..20]       ==  [10]
-- ---------------------------------------------------------------------

module Seleccion_con_fallo where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

seleccionConFallo1 :: (a -> Bool) -> [a] -> [a]
seleccionConFallo1 _ []                 = []
seleccionConFallo1 p (x:xs) | p x       = x : seleccionConFallo1 p xs
                            | otherwise = [x]

-- 2ª solución
-- ===========

seleccionConFallo2 :: (a -> Bool) -> [a] -> [a]
seleccionConFallo2 p xs = ys ++ take 1 zs
  where (ys,zs) = span p xs

-- 3ª solución
-- ===========

seleccionConFallo3 :: (a -> Bool) -> [a] -> [a]
seleccionConFallo3 = ((uncurry (++) . fmap (take 1)) .) . span

-- Ejemplo de cálculo:
--    seleccionConFallo (<5) [3,2,5,7,1,0]
--    = (((uncurry (++) . fmap (take 1)) .) . span) (<5) [3,2,5,7,1,0]
--    = (uncurry (++) . fmap (take 1)) ([3,2],[5,7,1,0])
--    = uncurry (++) ([3,2],[5])
--    = [3,2,5]

-- Nota: (fmap f (x,y)) es equivalente a (x,f y).

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Int -> Bool) -> [Int] -> [Int]) -> Spec
specG seleccionConFallo = do
  it "e1" $
    seleccionConFallo (<5) [3,2,5,7,1,0]  `shouldBe`  [3,2,5]
  it "e2" $
    seleccionConFallo odd [1..4]          `shouldBe`  [1,2]
  it "e3" $
    seleccionConFallo odd [1,3,5]         `shouldBe`  [1,3,5]
  it "e4" $
    seleccionConFallo (<5) [10..20]       `shouldBe`  [10]

spec :: Spec
spec = do
  describe "def. 1" $ specG seleccionConFallo1
  describe "def. 2" $ specG seleccionConFallo2
  describe "def. 3" $ specG seleccionConFallo3

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures
