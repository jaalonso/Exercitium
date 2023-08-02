-- BusquedaEnEscalada.hs
-- Búsqueda en escalada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-agosto-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En la búsqueda en escalada se supone que los estados están ordenados
-- mediante una función, la heurística, que es una estimación de su
-- coste para llegar a un estado final.
--
-- Definir la función
--    buscaEscalada :: Ord n => (n -> [n]) -> (n -> Bool) -> n -> [n]
-- tal que (buscaEscalada s o e) es la lista de soluciones del problema de
-- espacio de estado definido por la función sucesores s, el objetivo
-- o y estado inicial e, obtenidas buscando en escalada.
-- ---------------------------------------------------------------------

module BusquedaEnEscalada (buscaEscalada)  where

import TAD.ColaDePrioridad (esVacia, inserta, primero, vacia)

buscaEscalada :: Ord n => (n -> [n]) -> (n -> Bool) -> n -> [n]
buscaEscalada sucesores esFinal x = busca' (inserta x vacia) where
  busca' c
    | esVacia c           = []
    | esFinal (primero c) = [primero c]
    | otherwise           = busca' (foldr inserta vacia (sucesores (primero c)))
