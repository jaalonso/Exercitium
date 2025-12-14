-- Desemparejamiento_de_listas.hs
-- Desemparejamiento de listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-Diciembre-2014 (actualizado 14-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    desemparejada :: [(a,b)] -> ([a],[b])
-- tal que (desemparejada ps) es el par de lista (xs,ys) tal que al
-- emparejar (con zip) xs e ys devuelve ps. Por ejemplo,
--    λ> desemparejada [(3,'l'),(2,'u'),(5,'i'),(9,'s')]
--    ([3,2,5,9],"luis")
--
-- Comprobar con QuickCheck que
-- + desemparejada es equivalente a la función predefinida unzip.
-- + si el valor de (desemparejada ps) es (xs,ys), entonces (zip xs ys)
--   es igual a ps.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Desemparejamiento_de_listas where

import Data.List (foldl')
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

desemparejada1 :: [(a,b)] -> ([a],[b])
desemparejada1 ps = ([x | (x,_) <- ps], [y | (_,y) <- ps])

-- 2ª solución
-- ===========

desemparejada2 :: [(a,b)] -> ([a],[b])
desemparejada2 ps = (map fst ps, map snd ps)

-- 3ª solución
-- ===========

desemparejada3 :: [(a,b)] -> ([a],[b])
desemparejada3 []         = ([],[])
desemparejada3 ((x,y):ps) = (x:xs,y:ys)
  where (xs,ys) = desemparejada3 ps

-- 4ª solución
-- ===========

desemparejada4 :: [(a,b)] -> ([a],[b])
desemparejada4 = foldr f ([],[])
  where f (x,y) (xs,ys) = (x:xs, y:ys)

-- 5ª solución
-- ===========

desemparejada5 :: [(a,b)] -> ([a],[b])
desemparejada5 ps = (reverse us, reverse vs)
  where (us,vs) = foldl' f ([],[]) ps
        f (xs,ys) (x,y) = (x:xs,y:ys)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([(Int,Char)] -> ([Int],[Char])) -> Spec
specG desemparejada = do
  it "e1" $
    desemparejada [(3,'l'),(2,'u'),(5,'i'),(9,'s')]
    `shouldBe` ([3,2,5,9],"luis")

spec :: Spec
spec = do
  describe "def. 1" $ specG desemparejada1
  describe "def. 2" $ specG desemparejada2
  describe "def. 3" $ specG desemparejada3
  describe "def. 4" $ specG desemparejada4
  describe "def. 5" $ specG desemparejada5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: [(Int,Int)] -> Bool
prop_equivalencia ps =
  all (== desemparejada1 ps)
      [desemparejada2 ps,
       desemparejada3 ps,
       desemparejada4 ps,
       desemparejada5 ps]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ps = zip [1..10^7] [1..10^7]
--    λ> :set +s
--    λ> length (fst (desemparejada1 ps))
--    10000000
--    (1.93 secs, 2,960,603,592 bytes)
--    λ> length (fst (desemparejada2 ps))
--    10000000
--    (0.49 secs, 3,120,603,528 bytes)
--    λ> length (fst (desemparejada3 ps))
--    10000000
--    (3.45 secs, 5,520,603,512 bytes)
--    λ> length (fst (desemparejada4 ps))
--    *** Exception: stack overflow
--    λ> length (fst (desemparejada5 ps))
--    10000000
--    (5.79 secs, 4,560,603,784 bytes)

-- Propiedades
-- ===========

-- La primera propiedad es
prop_desemparejada_1 :: (Eq a, Eq b) => [(a,b)] -> Bool
prop_desemparejada_1 ps =
  desemparejada1 ps == unzip ps

-- Su comprobación es
--    λ> quickCheck prop_desemparejada_1
--    +++ OK, passed 100 tests.

-- La segunda propiedad es
prop_desemparejada_2 :: (Eq a, Eq b) => [(a,b)] -> Bool
prop_desemparejada_2 ps = zip xs ys == ps
  where (xs,ys) = desemparejada1 ps

-- Su comprobación es
--    λ> quickCheck prop_desemparejada_2
--    +++ OK, passed 100 tests.
