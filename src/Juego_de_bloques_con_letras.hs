-- Juego_de_bloques_con_letras.hs
-- Juego de bloques con letras.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-Diciembre-2014 (actualizado 8-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Para el juego de los bloques se dispone de un conjunto de bloques con
-- una letra en cada una de sus dos caras. El objetivo del juego
-- consiste en formar palabras sin que se pueda usar un bloque más de
-- una vez y sin diferenciar mayúsculas de minúsculas. Por ejemplo, si
-- se tiene tres bloques de forma que el 1º tiene las letras A y B, el
-- 2ª la N y la O y el 3º la O y la A entonces se puede obtener la
-- palabra ANA de dos formas: una con los bloques 1, 2 y 3 y otra con
-- los 3, 2 y 1.
--
-- Definir la función
--    soluciones :: [String] -> String -> [[String]]
-- tal que (soluciones bs cs) es la lista de las soluciones del juego de
-- los bloque usando los bloques bs (cada bloque es una cadena de dos
-- letras mayúsculas) para formar la palabra cs. Por ejemplo,
--    λ> soluciones ["AB","NO","OA"] "ANA"
--    [["AB","NO","OA"],["OA","NO","AB"]]
--    λ> soluciones ["AB","NE","OA"] "Bea"
--    [["AB","NE","OA"]]
--    λ> soluciones ["AB","NE","OA"] "EvA"
--    []
--    λ> soluciones ["AB","NO","OA","NC"] "ANA"
--    [["AB","NO","OA"],["AB","NC","OA"],["OA","NO","AB"],["OA","NC","AB"]]
--    λ> soluciones ["AB","NO","OA","NC"] "Anca"
--    [["AB","NO","NC","OA"],["OA","NO","NC","AB"]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Juego_de_bloques_con_letras where

import Data.List (delete, nubBy, sort)
import Data.Char (toUpper)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

soluciones1 :: [String] -> String -> [[String]]
soluciones1 bs cs = aux bs (map toUpper cs) []
  where
    aux :: [String] -> String -> [String] -> [[String]]
    aux _ [] _ = [[]]
    aux [] _ _ = []
    aux as (d:ds) usados =
      [a : rs | a <- as,
                a `notElem` usados,
                d `elem` a,
                rs <- aux as ds (a : usados)]

-- 2ª solución
-- ===========

soluciones2 :: [String] -> String -> [[String]]
soluciones2 _ [] = [[]]
soluciones2 [] _ = []
soluciones2 bs (c:cs) =
  [b:rs | b <- bs,
          toUpper c `elem` b,
          rs <- soluciones2 (delete b bs) cs]

-- 3ª solución
-- ===========

soluciones3 :: [String] -> String -> [[String]]
soluciones3 bs cs = aux bs' (map toUpper cs)
  where
    cs' = map toUpper cs
    bs' = filter (\b -> any (`elem` b) cs') bs
    aux _ [] = [[]]
    aux [] _ = []
    aux as (d:ds) =
      [a : rs | a <- as,
                d `elem` a,
                rs <- aux (delete a as) ds]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([String] -> String -> [[String]]) -> Spec
specG soluciones = do
  it "e1" $
    soluciones ["AB","NO","OA"] "ANA"
    `shouldBe` [["AB","NO","OA"],["OA","NO","AB"]]
  it "e2" $
    soluciones ["AB","NE","OA"] "Bea"
    `shouldBe` [["AB","NE","OA"]]
  it "e3" $
    soluciones ["AB","NE","OA"] "EvA"
    `shouldBe` []
  it "e4" $
    soluciones ["AB","NO","OA","NC"] "ANA"
    `shouldBe` [["AB","NO","OA"],["AB","NC","OA"],["OA","NO","AB"],["OA","NC","AB"]]
  it "e5" $
    soluciones ["AB","NO","OA","NC"] "Anca"
    `shouldBe` [["AB","NO","NC","OA"],["OA","NO","NC","AB"]]

spec :: Spec
spec = do
  describe "def. 1" $ specG soluciones1
  describe "def. 2" $ specG soluciones2
  describe "def. 3" $ specG soluciones3

-- La verificación es
--    λ> verifica
--    15 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Generador de una letra mayúscula. Por ejemplo,
--    λ> generate genLetra
--    'O'
--    λ> generate genLetra
--    'H'
genLetra :: Gen Char
genLetra = elements ['A'..'Z']

-- Generador de un bloque de dos letras mayúsculas. Por ejemplo,
--    λ> generate genBloque
--    "ZK"
--    λ> generate genBloque
--    "JW"
genBloque :: Gen String
genBloque = do
  c1 <- genLetra
  c2 <- genLetra
  return [c1, c2]

-- (distintos bs) es la lista obtenida eliminando en bs los bloques
-- iguales. Por ejemplo,
--    λ> distintos ["AB", "BC", "BA"]
--    ["AB","BC"]
distintos :: [String] -> [String]
distintos = nubBy (\b1 b2 -> sort b1 == sort b2)

-- Generador de lista de bloques distintos y una palabra objetivo. Por
-- ejemplo,
--    λ> generate genEntrada
--    (["UQ","MB","TP","YV","GC","SY","AD","VN","TU"],"")
--    λ> generate genEntrada
--    (["ZX","OP","XX","SN","MD","HO","KR","NV"],"MTQ")
genEntrada :: Gen ([String], String)
genEntrada = do
  n <- choose (0, 10)
  bs <- vectorOf n genBloque
  m <- choose (0, 5)
  cs <- vectorOf m genLetra
  return (distintos bs, cs)

-- La propiedad es
prop_equivalencia :: Property
prop_equivalencia = forAll genEntrada $ \(bs, cs) ->
  all (== sort (soluciones1 bs cs))
      [sort (soluciones2 bs cs),
       sort (soluciones3 bs cs)]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

bloques :: [String]
bloques = [[x,y] | x <- ['A'..'Z'], y <- [succ x..'Z']]

-- La comparación es
--    λ> length (soluciones1 bloques "ABCD")
--    386878
--    (2.50 secs, 1,307,186,032 bytes)
--    λ> length (soluciones2 bloques "ABCD")
--    386878
--    (1.96 secs, 892,974,224 bytes)
--    λ> length (soluciones3 bloques "ABCD")
--    386878
--    (0.67 secs, 455,688,320 bytes)
