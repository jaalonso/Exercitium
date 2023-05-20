-- GrafoConVectorDeAdyacencia.hs
-- Representación del TAD grafo mediante vectores de adyacencia.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-mayo-2023
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE FlexibleInstances #-}

module TAD.GrafoConVectorDeAdyacencia
  (Orientacion (..),
   Grafo,
   creaGrafo,
   creaGrafo',
   dirigido,
   adyacentes,
   nodos,
   aristas,
   aristaEn,
   peso
  ) where

-- Librerías auxiliares
import Data.Array (Array, Ix, accumArray, indices, (!))
import Data.List (sort)
import Test.QuickCheck

-- Orientacion es D (dirigida) ó ND (no dirigida).
data Orientacion = D | ND
  deriving (Eq, Show)

-- (Grafo v p) es un grafo con vértices de tipo v y pesos de tipo p.
data Grafo v p = G Orientacion (Array v [(v,p)])
  deriving Eq

-- (escribeGrafo g) es la cadena correspondiente al grafo g. Por
-- ejemplo,
--    λ> escribeGrafo (creaGrafo ND (1,3) [(1,2,0),(2,3,5),(2,2,0)])
--    "G ND ([1,2,3],[((1,2),0),((2,2),0),((2,3),5)])"
--    λ> escribeGrafo (creaGrafo D (1,3) [(1,2,0),(2,3,5),(2,2,0)])
--    "G D ([1,2,3],[((1,2),0),((2,2),0),((2,3),5)])"
--    λ> escribeGrafo (creaGrafo ND (1,3) [(1,2,0),(2,3,0),(2,2,0)])
--    "G ND ([1,2,3],[(1,2),(2,2),(2,3)])"
--    λ> escribeGrafo (creaGrafo D (1,3) [(1,2,0),(2,3,0),(2,2,0)])
--    "G D ([1,2,3],[(1,2),(2,2),(2,3)])"
--    λ> escribeGrafo (creaGrafo D (1,3) [(1,2,0),(3,2,0),(2,2,0)])
--    "G D ([1,2,3],[(1,2),(2,2),(3,2)])"
--    λ> escribeGrafo (creaGrafo ND (1,3) [(1,2,0),(3,2,0),(2,2,0)])
--    "G ND ([1,2,3],[(1,2),(2,2),(2,3)])"
escribeGrafo :: (Ix v,Num p,Ord v, Ord p,Show v,Show p) => Grafo v p -> String
escribeGrafo g@(G o _) =
  "G " ++ show o ++ " (" ++ show vs ++ "," ++ escribeAristas ++ ")"
  where
    as = sort (aristas g)
    vs = nodos g
    aristasReducidas
      | o == D    = as
      | otherwise = [((x,y),p) | ((x,y),p) <- as, x <= y]
    escribeAristas
      | ponderado = show aristasReducidas
      | otherwise = show [a | (a,_) <- aristasReducidas]
    ponderado = any (\((_,_),p) -> p /= 0) as

-- Procedimiento de escritura de grafos
instance (Ix v,Num p,Ord v, Ord p,Show v,Show p) => Show (Grafo v p) where
  show = escribeGrafo

-- (creaGrafo o cs as) es un grafo (dirigido o no, según el valor de o),
-- con el par de cotas cs y listas de aristas as (cada arista es un trío
-- formado por los dos vértices y su peso). Ver el ejemplo a continuación.
creaGrafo :: (Ix v, Num p) => Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p
creaGrafo o cs vs =
  G o (accumArray (\xs x -> xs++[x]) [] cs
                  ((if o == D then []
                    else [(x2,(x1,p))|(x1,x2,p) <- vs, x1 /= x2]) ++
                   [(x1,(x2,p)) | (x1,x2,p) <- vs]))

-- (creaGrafo' o cs as) es un grafo (dirigido o no, según el valor de o),
-- con el par de cotas cs y listas de aristas as (cada arista es un par
-- de vértices y se supone que su peso es 0). Por ejemplo,
--    λ> creaGrafo' ND (1,3) [(2,1),(1,3)]
--    G ND ([1,2,3],[(1,2),(1,3)])
--    λ> creaGrafo' D (1,3) [(2,1),(1,3)]
--    G D ([1,2,3],[(1,3),(2,1)])
creaGrafo' :: (Ix v, Num p, Ord v, Ord p) =>
              Orientacion -> (v,v) -> [(v,v)] -> Grafo v p
creaGrafo' o cs as =
  creaGrafo o cs [(v1,v2,0) | (v1,v2) <- as]

-- ejGrafoND es el grafo
--             12
--        1 -------- 2
--        | \78     /|
--        |  \   32/ |
--        |   \   /  |
--      34|     5    |55
--        |   /   \  |
--        |  /44   \ |
--        | /     93\|
--        3 -------- 4
--             61
-- representado mediante un vector de adyacencia; es decir,
--    λ> ejGrafoND
--    G ND ([1,2,3,4,5],
--          [((1,2),12),((1,3),34),((1,5),78),
--           ((2,4),55),((2,5),32),
--           ((3,4),61),((3,5),44),
--           ((4,5),93)])
ejGrafoND :: Grafo Int Int
ejGrafoND = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]

-- ejGrafoD es el mismo grafo que ejGrafoND pero orientando las aristas;
-- es decir,
--    λ> ejGrafoD
--    G D ([1,2,3,4,5],
--         [((1,2),12),((1,3),34),((1,5),78),
--          ((2,4),55),((2,5),32),
--          ((3,4),61),((3,5),44),
--          ((4,5),93)])
ejGrafoD :: Grafo Int Int
ejGrafoD = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                              (2,4,55),(2,5,32),
                              (3,4,61),(3,5,44),
                              (4,5,93)]

-- (dirigido g) se verifica si g es dirigido. Por ejemplo,
--    dirigido ejGrafoD   ==  True
--    dirigido ejGrafoND  ==  False
dirigido :: (Ix v,Num p) => Grafo v p -> Bool
dirigido (G o _) = o == D

-- (nodos g) es la lista de todos los nodos del grafo g. Por ejemplo,
--    nodos ejGrafoND  ==  [1,2,3,4,5]
--    nodos ejGrafoD   ==  [1,2,3,4,5]
nodos :: (Ix v,Num p) => Grafo v p -> [v]
nodos (G _ g) = indices g

-- (adyacentes g v) es la lista de los vértices adyacentes al nodo v en
-- el grafo g. Por ejemplo,
--    adyacentes ejGrafoND 4  ==  [2,3,5]
--    adyacentes ejGrafoD  4  ==  [5]
adyacentes :: (Ix v,Num p) => Grafo v p -> v -> [v]
adyacentes (G _ g) v = map fst (g!v)

-- (aristaEn g a) se verifica si a es una arista del grafo g. Por
-- ejemplo,
--    aristaEn ejGrafoND (5,1)  ==  True
--    aristaEn ejGrafoND (4,1)  ==  False
--    aristaEn ejGrafoD (5,1)   ==  False
--    aristaEn ejGrafoD (1,5)   ==  True
aristaEn :: (Ix v,Num p) => Grafo v p -> (v,v) -> Bool
aristaEn g (x,y) = y `elem` adyacentes g x

-- (peso v1 v2 g) es el peso de la arista que une los vértices v1 y v2
-- en el grafo g. Por ejemplo,
--    peso 1 5 ejGrafoND  ==  78
--    peso 1 5 ejGrafoD   ==  78
peso :: (Ix v,Num p) => v -> v -> Grafo v p -> p
peso x y (G _ g) = head [c | (a,c) <- g!x , a == y]

-- (aristas g) es la lista de las aristas del grafo g. Por ejemplo,
--    λ> aristas ejGrafoD
--    [((1,2),12),((1,3),34),((1,5),78),
--     ((2,4),55),((2,5),32),
--     ((3,4),61),((3,5),44),
--     ((4,5),93)]
--    λ> aristas ejGrafoND
--    [((1,2),12),((1,3),34),((1,5),78),
--     ((2,1),12),((2,4),55),((2,5),32),
--     ((3,1),34),((3,4),61),((3,5),44),
--     ((4,2),55),((4,3),61),((4,5),93),
--     ((5,1),78),((5,2),32),((5,3),44),((5,4),93)]
aristas :: (Ix v,Num p) => Grafo v p -> [((v,v),p)]
aristas (G o g) = [((v1,v2),w) | v1 <- nodos (G o g) , (v2,w) <- g!v1]

-- ---------------------------------------------------------------------
-- § Generador de grafos                                              --
-- ---------------------------------------------------------------------

-- (generaGND n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    λ> generaGND 3 [4,2,5]
--    G ND ([1,2,3],[((1,2),4),((1,3),2),((2,3),5)])
--    λ> generaGND 3 [4,-2,5]
--    G ND ([1,2,3],[((1,2),4),((2,3),5)])
generaGND :: Int -> [Int] -> Grafo Int Int
generaGND n ps  = creaGrafo ND (1,n) l3
  where l1 = [(x,y) | x <- [1..n], y <- [1..n], x < y]
        l2 = zip l1 ps
        l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- (generaGD n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    λ> generaGD 3 [4,2,5]
--    G D ([1,2,3],[((1,1),4),((1,2),2),((1,3),5)])
--    λ> generaGD 3 [4,2,5,3,7,9,8,6]
--    G D ([1,2,3],[((1,1),4),((1,2),2),((1,3),5),
--                  ((2,1),3),((2,2),7),((2,3),9),
--                  ((3,1),8),((3,2),6)])
generaGD :: Int -> [Int] -> Grafo Int Int
generaGD n ps = creaGrafo D (1,n) l3
  where l1 = [(x,y) | x <- [1..n], y <- [1..n]]
        l2 = zip l1 ps
        l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- genGD es un generador de grafos dirigidos. Por ejemplo,
--    λ> sample genGD
--    G D ([1],[])
--    G D ([1,2],[((1,1),5),((2,1),4)])
--    G D ([1,2],[((1,1),3),((1,2),3)])
--    G D ([1,2,3,4,5,6],[])
--    G D ([1,2],[((2,2),16)])
--    ...
genGD :: Gen (Grafo Int Int)
genGD = do
  n <- choose (1,10)
  xs <- vectorOf (n*n) arbitrary
  return (generaGD n xs)

-- genGND es un generador de grafos dirigidos. Por ejemplo,
--    λ> sample genGND
--    G ND ([1,2,3,4,5,6,7,8],[])
--    G ND ([1],[])
--    G ND ([1,2,3,4,5],[((1,2),2),((2,3),5),((3,4),5),((3,5),5)])
--    G ND ([1,2,3,4,5],[((1,2),6),((1,3),5),((1,5),1),((3,5),9),((4,5),6)])
--    G ND ([1,2,3,4],[((1,2),5),((3,4),2)])
--    G ND ([1,2,3],[])
--    G ND ([1,2,3,4],[((1,2),5),((1,4),14),((2,4),10)])
--    G ND ([1,2,3,4,5],[((1,5),8),((4,5),5)])
--    G ND ([1,2,3,4],[((1,2),1),((1,4),4),((2,3),4),((3,4),5)])
--    G ND ([1,2,3],[((1,2),8),((1,3),8),((2,3),3)])
--    ...
genGND :: Gen (Grafo Int Int)
genGND = do
  n <- choose (1,10)
  xs <- vectorOf (n*n) arbitrary
  return (generaGND n xs)

-- genG es un generador de grafos. Por ejemplo,
--    λ> sample genG
--    G ND ([1,2,3,4,5,6],[])
--    G D ([1],[((1,1),2)])
--    G D ([1,2],[((1,1),9)])
--    ...
genG :: Gen (Grafo Int Int)
genG = do
  d <- choose (True,False)
  n <- choose (1,10)
  xs <- vectorOf (n*n) arbitrary
  if d then return (generaGD n xs)
       else return (generaGND n xs)

-- Los grafos está contenido en la clase de los objetos generables
-- aleatoriamente.
instance Arbitrary (Grafo Int Int) where
  arbitrary = genG
