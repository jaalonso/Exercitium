-- Ordenacion_de_estructuras.hs
-- Ordenación de estructuras.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-mayo-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las notas de los dos primeros exámenes se pueden representar mediante
-- el siguiente tipo de dato
--    data Notas = Notas String Int Int
--      deriving (Read, Show, Eq)
-- Por ejemplo, (Notas "Juan" 6 5) representar las notas de un alumno
-- cuyo nombre es Juan, la nota del primer examen es 6 y la del segundo
-- es 5.
--
-- Definir la función
--    ordenadas :: [Notas] -> [Notas]
-- tal que (ordenadas ns) es la lista de las notas ns ordenadas
-- considerando primero la nota del examen 2, a continuación la del
-- examen 1 y finalmente el nombre. Por ejemplo,
--    λ> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 7]
--    [Notas "Juan" 6 5,Notas "Luis" 3 7]
--    λ> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 4]
--    [Notas "Luis" 3 4,Notas "Juan" 6 5]
--    λ> ordenadas [Notas "Juan" 6 5, Notas "Luis" 7 4]
--    [Notas "Luis" 7 4,Notas "Juan" 6 5]
--    λ> ordenadas [Notas "Juan" 6 4, Notas "Luis" 7 4]
--    [Notas "Juan" 6 4,Notas "Luis" 7 4]
--    λ> ordenadas [Notas "Juan" 6 4, Notas "Luis" 5 4]
--    [Notas "Luis" 5 4,Notas "Juan" 6 4]
--    λ> ordenadas [Notas "Juan" 5 4, Notas "Luis" 5 4]
--    [Notas "Juan" 5 4,Notas "Luis" 5 4]
--    λ> ordenadas [Notas "Juan" 5 4, Notas "Eva" 5 4]
--    [Notas "Eva" 5 4,Notas "Juan" 5 4]
-- ---------------------------------------------------------------------

module A2014.M05.Ordenacion_de_estructuras where

import Data.List (sort, sortBy)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Notas = Notas String Int Int
  deriving (Read, Show, Eq)

-- 1ª solución
-- ===========

ordenadas1 :: [Notas] -> [Notas]
ordenadas1 ns =
  [Notas n x y | (y,x,n) <- sort [(y1,x1,n1) | (Notas n1 x1 y1) <- ns]]

-- 2ª solución
-- ===========

ordenadas2 :: [Notas] -> [Notas]
ordenadas2 ns =
  map (\(y,x,n) -> Notas n x y) (sort [(y1,x1,n1) | (Notas n1 x1 y1) <- ns])

-- 3ª solución
-- ===========

ordenadas3 :: [Notas] -> [Notas]
ordenadas3 = sortBy (\(Notas n1 x1 y1) (Notas n2 x2 y2) ->
                        compare (y1,x1,n1) (y2,x2,n2))

-- 4ª solución
-- ===========

instance Ord Notas where
  Notas n1 x1 y1 <= Notas n2 x2 y2 = (y1,x1,n1) <= (y2,x2,n2)

ordenadas4 :: [Notas] -> [Notas]
ordenadas4 = sort

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Notas] -> [Notas]) -> Spec
specG ordenadas = do
  it "e1" $
    ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 7] `shouldBe`
      [Notas "Juan" 6 5,Notas "Luis" 3 7]
  it "e2" $
    ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 4] `shouldBe`
      [Notas "Luis" 3 4,Notas "Juan" 6 5]
  it "e3" $
    ordenadas [Notas "Juan" 6 5, Notas "Luis" 7 4] `shouldBe`
      [Notas "Luis" 7 4,Notas "Juan" 6 5]
  it "e4" $
    ordenadas [Notas "Juan" 6 4, Notas "Luis" 7 4] `shouldBe`
      [Notas "Juan" 6 4,Notas "Luis" 7 4]
  it "e5" $
    ordenadas [Notas "Juan" 6 4, Notas "Luis" 5 4] `shouldBe`
      [Notas "Luis" 5 4,Notas "Juan" 6 4]
  it "e6" $
    ordenadas [Notas "Juan" 5 4, Notas "Luis" 5 4] `shouldBe`
      [Notas "Juan" 5 4,Notas "Luis" 5 4]
  it "e7" $
    ordenadas [Notas "Juan" 5 4, Notas "Eva" 5 4] `shouldBe`
      [Notas "Eva" 5 4,Notas "Juan" 5 4]

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenadas1
  describe "def. 2" $ specG ordenadas2
  describe "def. 3" $ specG ordenadas3
  describe "def. 4" $ specG ordenadas4

-- La verificación es
--    λ> verifica
--    28 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- notasArbitraria es un generador aleatorio de notas. Por ejemplo,
--    λ> sample notasArbitraria
--    Notas "achjkqruvxy" 3 3
--    Notas "abfgikmptuvy" 10 10
--    Notas "degjmptvwx" 7 9
--    Notas "cdefghjmnoqrsuw" 0 9
--    Notas "bcdfikmstuxz" 1 8
--    Notas "abcdhkopqsvwx" 10 7
--    Notas "abghiklnoqstvwx" 0 0
--    Notas "abfghklmnoptuvx" 4 9
--    Notas "bdehjkmpqsxyz" 0 4
--    Notas "afghijmopsvwz" 3 7
--    Notas "bdefghjklnoqx" 2 3
notasArbitraria :: Gen Notas
notasArbitraria = do
  n <- sublistOf ['a'..'z']
  x <- chooseInt (0, 10)
  y <- chooseInt (0, 10)
  return (Notas n x y)

-- Notas es una subclase de Arbitrary
instance Arbitrary Notas where
  arbitrary = notasArbitraria

-- La propiedad es
prop_ordenadas :: [Notas] -> Bool
prop_ordenadas ns =
  all (== ordenadas1 ns)
      [f ns | f <- [ordenadas2,
                    ordenadas3,
                    ordenadas4]]

-- La comprobación es
--    λ> quickCheck prop_ordenadas
--    +++ OK, passed 100 tests.
