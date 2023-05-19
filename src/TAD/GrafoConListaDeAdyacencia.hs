-- GrafoConListaDeAdyacencia.hs
-- Implementación del TAD de los grafos mediante listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-mayo-2023
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE FlexibleInstances #-}

module TAD.GrafoConListaDeAdyacencia
    (Orientacion (..),
     Grafo,
     creaGrafo,  -- (Ix v, Num p, Ord v, Ord p) =>
                 -- Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p
     creaGrafo', -- (Ix v, Num p, Ord v, Ord p) =>
                 -- Orientacion -> (v,v) -> [(v,v)] -> Grafo v p
     dirigido,   -- (Ix v,Num p) => (Grafo v p) -> Bool
     adyacentes, -- (Ix v,Num p) => (Grafo v p) -> v -> [v]
     nodos,      -- (Ix v,Num p) => (Grafo v p) -> [v]
     aristas,    -- (Ix v,Num p) => (Grafo v p) -> [((v,v),p)]
     aristaEn,   -- (Ix v,Num p) => (Grafo v p) -> (v,v) -> Bool
     peso        -- (Ix v,Num p) => v -> v -> (Grafo v p) -> p
    ) where

-- Librerías auxiliares                                               --
import Data.Array
import Data.List

-- Orientacion es D (dirigida) ó ND (no dirigida).
data Orientacion = D | ND
  deriving (Eq, Show)

-- (Grafo v p) es un grafo con vértices de tipo v y pesos de tipo p.
data Grafo v p = G Orientacion ([v],[((v,v),p)])
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
escribeGrafo :: (Ix v,Num p,Eq p,Show v,Show p) => Grafo v p -> String
escribeGrafo (G o (vs,as)) =
  "G " ++ show o ++ " (" ++ show vs ++ "," ++ escribeAristas ++ ")"
  where
    aristasReducidas
      | o == D    = as
      | otherwise = [((x,y),p) | ((x,y),p) <- as, x <= y]
    escribeAristas
      | ponderado = show aristasReducidas
      | otherwise = show [a | (a,_) <- aristasReducidas]
    ponderado = any (\((_,_),p) -> p /= 0) as

-- Procedimiento de escritura de grafos
instance (Ix v,Num p,Eq p,Show v,Show p) => Show (Grafo v p) where
  show = escribeGrafo

-- (creaGrafo o cs as) es un grafo (dirigido o no, según el valor de o),
-- con el par de cotas cs y listas de aristas as (cada arista es un trío
-- formado por los dos vértices y su peso). Por ejemplo,
--    λ> creaGrafo ND (1,3) [(1,2,12),(1,3,34)]
--    G ND ([1,2,3],[((1,2),12),((1,3),34),((2,1),12),((3,1),34)])
--    λ> creaGrafo D (1,3) [(1,2,12),(1,3,34)]
--    G D ([1,2,3],[((1,2),12),((1,3),34)])
--    λ> creaGrafo D (1,4) [(1,2,12),(1,3,34)]
--    G D ([1,2,3,4],[((1,2),12),((1,3),34)])
creaGrafo :: (Ix v, Num p, Ord v, Ord p) =>
             Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p
creaGrafo o cs as =
  G o (range cs,
       sort ([((x1,x2),p) | (x1,x2,p) <- as] ++
             if o == D then []
             else [((x2,x1),p) | (x1,x2,p) <- as, x1 /= x2]))

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
-- Se define por
ejGrafoND :: Grafo Int Int
ejGrafoND = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]

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
nodos (G _ (ns,_)) = ns

-- (adyacentes g v) es la lista de los vértices adyacentes al nodo v en
-- el grafo g. Por ejemplo,
--    adyacentes ejGrafoND 4  ==  [2,3,5]
--    adyacentes ejGrafoD  4  ==  [5]
adyacentes :: (Ix v, Num p) => Grafo v p -> v -> [v]
adyacentes (G _ (_,e)) v = nub [u | ((w,u),_) <- e, w == v]

-- (aristaEn g a) se verifica si a es una arista del grafo g. Por
-- ejemplo,
--    aristaEn ejGrafoND (5,1)  ==  True
--    aristaEn ejGrafoND (4,1)  ==  False
--    aristaEn ejGrafoD  (5,1)  ==  False
--    aristaEn ejGrafoD  (1,5)  ==  True
aristaEn :: (Ix v,Num p) => Grafo v p -> (v,v) -> Bool
aristaEn g (x,y) = y `elem` adyacentes g x

-- (peso v1 v2 g) es el peso de la arista que une los vértices v1 y v2
-- en el grafo g. Por ejemplo,
--    peso 1 5 ejGrafoND  ==  78
--    peso 1 5 ejGrafoD   ==  78
peso :: (Ix v,Num p) => v -> v -> Grafo v p -> p
peso x y (G _ (_,gs)) = head [c | ((x',y'),c) <- gs, (x,y) == (x',y')]

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
aristas (G _ (_,g)) = [((v1,v2),p) | ((v1,v2),p) <- g]
