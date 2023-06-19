-- DivideVenceras.hs
-- Algoritmo divide y vencerás
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La técnica [divide y vencerás](https://bit.ly/46afaca) consta de
-- los siguientes pasos:
-- + Dividir el problema en subproblemas menores.
-- + Resolver por separado cada uno de los subproblemas:
--   + si los subproblemas son complejos, usar la misma técnica recursivamente;
--   + si son simples, resolverlos directamente.
-- + Combinar todas las soluciones de los subproblemas en una solución simple.
--
-- Definir la función
--    divideVenceras :: (p -> Bool)
--                   -> (p -> s)
--                   -> (p -> [p])
--                   -> (p -> [s] -> s)
--                   -> p
--                   -> s
-- tal que (divideVenceras ind resuelve divide combina pbInicial)
-- resuelve el problema pbInicial mediante la técnica de divide y
-- vencerás, donde
-- + (ind pb) se verifica si el problema pb es indivisible
-- + (resuelve pb) es la solución del problema indivisible pb
-- + (divide pb) es la lista de subproblemas de pb
-- + (combina pb ss) es la combinación de las soluciones ss de los
--   subproblemas del problema pb.
-- + pbInicial es el problema inicial
--
-- Usando la función DivideVenceras definir las funciones
--    ordenaPorMezcla :: Ord a => [a] -> [a]
--    ordenaRapida    :: Ord a => [a] -> [a]
-- tales que
-- + (ordenaPorMezcla xs) es la lista obtenida ordenando xs por el
--   procedimiento de ordenación por mezcla. Por ejemplo,
--      λ> ordenaPorMezcla [3,1,4,1,5,9,2,8]
--      [1,1,2,3,4,5,8,9]
-- + (ordenaRapida xs) es la lista obtenida ordenando xs por el
--   procedimiento de ordenación rápida. Por ejemplo,
--      λ> ordenaRapida [3,1,4,1,5,9,2,8]
--      [1,1,2,3,4,5,8,9]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module DivideVenceras (divideVenceras) where

import Test.Hspec (Spec, hspec, it, shouldBe)

divideVenceras :: (p -> Bool)
               -> (p -> s)
               -> (p -> [p])
               -> (p -> [s] -> s)
               -> p
               -> s
divideVenceras ind resuelve divide combina = dv'
  where
    dv' pb
      | ind pb    = resuelve pb
      | otherwise = combina pb [dv' sp | sp <- divide pb]

ordenaPorMezcla :: Ord a => [a] -> [a]
ordenaPorMezcla =
    divideVenceras ind id divide combina
    where
      ind xs            = length xs <= 1
      divide xs         = [take n xs, drop n xs]
                          where n = length xs `div` 2
      combina _ [l1,l2] = mezcla l1 l2

-- (mezcla xs ys) es la lista obtenida mezclando xs e ys. Por ejemplo,
--    mezcla [1,3] [2,4,6]  ==  [1,2,3,4,6]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] b = b
mezcla a [] = a
mezcla a@(x:xs) b@(y:ys) | x <= y    = x : mezcla xs b
                         | otherwise = y : mezcla a ys

ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida =
    divideVenceras ind id divide combina
    where
      ind xs                = length xs <= 1
      divide (x:xs)         = [[ y | y <- xs, y <= x],
                               [ y | y <- xs, y > x]]
      combina (x:_) [l1,l2] = l1 ++ [x] ++ l2

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    ordenaPorMezcla [3,1,4,1,5,9,2,8] `shouldBe` [1,1,2,3,4,5,8,9]
  it "e2" $
    ordenaRapida [3,1,4,1,5,9,2,8] `shouldBe` [1,1,2,3,4,5,8,9]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0004 seconds
--    2 examples, 0 failures
