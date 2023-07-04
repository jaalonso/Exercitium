-- BusquedaPrimeroElMejor.hs
-- Búsqueda por primero el mejor.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-julio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En la búsqueda por primero el mejos se supone que los estados están
-- ordenados mediante una función, la heurística, que es una rstimación
-- de su coste para llegar a un estado final.
--
-- Definir la función
--    buscaPM :: Ord n => (n -> [n]) -> (n -> Bool) -> n -> [n]
-- tal que (buscaPM s o e) es la lista de soluciones del problema de
-- espacio de estado definido por la función sucesores s, el objetivo
-- o y estado inicial e, obtenidas buscando por primero el mejor.
-- ---------------------------------------------------------------------

module BusquedaPrimeroElMejor (buscaPM)  where

import TAD.ColaDePrioridad

buscaPM :: Ord n => (n -> [n]) -> (n -> Bool) -> n -> [n]
buscaPM sucesores esFinal x = busca' (inserta x vacia) where
  busca' c
    | esVacia c = []
    | esFinal (primero c) = primero c : busca' (resto c)
    | otherwise = busca' (foldr inserta (resto c) (sucesores (primero c)))
