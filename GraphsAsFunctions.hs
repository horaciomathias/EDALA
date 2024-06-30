module GraphsAsFunctions where
import Data.List
import Data.Maybe
--Grafos
type N = Integer --consideraremos solo a los naturales.
type Conj a = [a]
type V = N
type G = V -> Conj V
type Gn = (G,N) -> G
type A = (V,V)
type AP = (V, V, N) -- esto o la aplicacion de la funcion de costo con cada arista
type C = A -> N
type GP = (Gn, C)

--Ejemplos mencionados anteriormente
gid :: G
gid v = [v]

--Necesaria para obtener los vertices del grafo
vertices :: G -> Conj V
vertices g = [0..]

gleq :: G
gleq v = [x | x <- vertices gleq, x <= v]

gs :: G
gs v = [v+1]

geven :: G
geven v = [x | x <- vertices geven, even x]

geo :: G
geo v
    | even v = [e | even v, e <- vertices geo]
    | otherwise = [x | x <- vertices geo, odd x]

gfin :: G
gfin 0 = [1]
gfin 1 = []
gfin 2 = [3,4]
gfin 3 = [2]
gfin 4 = [4,5]
gfin 5 = [2,3,5]
gfin _ = []

--grafo ponderado
gp :: GP
gp = (gn, c)
  where
    gn (g, n) v = g v
    c (v1, v2) = if v1 == v2 then 0 else 1

--Definir un Gn
gn :: Gn
gn (g,n) v = if v < n then g v else undefined

--Definir una funcion de costo
c :: C
c (v1, v2) = if v1 == v2 then 0 else 2

--Definir un grafo ponderado con el gn y c
gp2 :: GP
gp2 = (gn, c)

geo2 :: G
geo2 v
    | even v = [e | even v, e <- vertices geo2]
    | otherwise = [x | x <- vertices geo2, odd x]

geo5 :: G
geo5 = gn (geo, 5)

existeArista :: A -> G -> Bool
existeArista (v1, v2) g = v2 `elem` g v1

tieneLazoV :: V -> G -> Bool
tieneLazoV v g = v `elem` g v

esAislado :: V -> G -> Bool
esAislado v g = null (g v)

existeSimetrica :: A -> G -> Bool
--existeSimetrica (v1, v2) g = v1 `elem` g v2
existeSimetrica (v1, v2) g = v2 `elem` g v1 && v1 `elem` g v2

incidentes :: V -> [V] -> G -> N
incidentes v [] g =  0
incidentes v [v1] g = if v `elem` g v1 then 1 else 0
incidentes v (v1 : vs) g = if v `elem` g v1 then 1 + incidentes v vs g else incidentes v vs g
--incidentes :: V -> [V] -> G -> Int
--incidentes v vs g = length [u | u <- vs, v `elem` (g u)]

gradoEntrada :: V -> G -> N
gradoEntrada v g = incidentes v (vertices g) g

gradoSalida :: V -> G -> N
gradoSalida v g = fromIntegral (length (g v))

gradoV :: V -> G -> N
gradoV v g = gradoEntrada v g + gradoSalida v g

vertices2 :: G -> [V]
vertices2 g = [v | v <- [0..], not (null (g v))]
--vertices2 g = [v | v <- [0..5], not (null (g v))]
--devuelve una lista de todos los vértices del grafo g que tienen al menos un vértice adyacente. 
--Itera sobre los posibles vértices [0..] y selecciona aquellos que no tienen una lista de adyacencia vacía en el grafo g.

aristas :: G -> [A]
aristas g = [(v1, v2) | v1 <- vertices2 g, v2 <- g v1]

grado :: G -> Bool
grado g = all (\v -> gradoEntrada v g == gradoSalida v g) (vertices2 g)
--verifica si, en un grafo g, el grado de entrada es igual al grado de salida para todos los vértices del grafo. 
--Implica que cada vértice tiene el mismo número de aristas entrando y saliendo de él.

listaDeAdyacencia :: G -> [(V,[V])]
--listaDeAdyacencia g = [(v, g v) | v <- vertices2 g] --queda en loop infinito
listaDeAdyacencia g = [(v, g v) | v <- verticesLim g]
  where
    verticesLim g = takeWhile (not . null . g) [0..10] -- Limitar a 10 para evitar loops infinitos en la prueba

matrizDeAdyacencia :: G -> [[Bool]]
matrizDeAdyacencia g = [[v2 `elem` g v1 | v2 <- vertices] | v1 <- vertices]
  where vertices = vertices2 g


parVA :: G -> ([V],[A])
parVA g = (vs, as)
  where
    vs = vertices2 g
    as = aristas g
--La función parVA toma un grafo g y devuelve una tupla que contiene dos listas:
--  -Una lista de vértices [V] del grafo.
--  -Una lista de aristas [A] del grafo.

agregarA :: A -> G -> G
agregarA (v1, v2) g v = if v == v1 then v2 : g v else g v

sacarA :: A -> G -> G
sacarA (v1, v2) g v = if v == v1 then filter (/= v2) (g v) else g v

esSubgrafo :: G -> G -> Bool
esSubgrafo g1 g2 = all (\v -> all (`elem` g2 v) (g1 v)) (vertices2 g1)

sonComplementarios :: G -> G -> Bool
sonComplementarios g1 g2 = all (\v -> g1 v == complementario g2 v) (vertices2 g1)
  where
    complementario g v = [u | u <- vertices2 g, u /= v, u `notElem` g v]

complementario :: G -> G
complementario g v = [u | u <- vertices2 g, u /= v, u `notElem` g v]

esCamino :: [V] -> G -> [A]
esCamino [] _ = []
esCamino [_] _ = []
esCamino (v1:v2:vs) g
  | v2 `elem` g v1 = (v1, v2) : esCamino (v2:vs) g
  | otherwise = []

clausuraSimetrica :: G -> G
clausuraSimetrica g v = g v ++ [u | u <- vertices2 g, v `elem` g u, u `notElem` g v]
-- Toma un grafo g y devuelve un nuevo grafo en el que, para cada vértice v, se asegura que si v está conectado a u, entonces u también está conectado a v. 
-- En otras palabras, agrega las aristas necesarias para hacer que las relaciones sean simétricas.

clausuraTransitiva :: G -> G
clausuraTransitiva g v = cierreTransitivo [v] [] g
-- toma un grafo g y devuelve un nuevo grafo, donde para cada vértice v, se obtiene la lista de vértices accesibles desde v usando la función auxiliar cierreTransitivo.

cierreTransitivo :: [V] -> [V] -> G -> [V]
cierreTransitivo [] cerrados _ = cerrados
cierreTransitivo (x:xs) cerrados g
  | x `elem` cerrados = cierreTransitivo xs cerrados g
  | otherwise = cierreTransitivo (xs ++ g x) (x:cerrados) g

esCiclico :: G -> Bool
esCiclico g = any (\v -> v `elem` cierreTransitivo [v] [] g) (vertices2 g)

esConexo :: G -> Bool
--esConexo g = all (\v -> length (cierreTransitivo [v] [] g) == length (vertices2 g)) (vertices2 g)
esConexo g = length (dfs g) == length (vertices2 g)

ordenTopologico :: G -> [V]
ordenTopologico g = reverse (topoSort (vertices2 g) [])
  where
    topoSort [] visitados = visitados
    topoSort (x:xs) visitados
      | x `elem` visitados = topoSort xs visitados
      | otherwise = topoSort xs (dfsVisit g x visitados)

    dfsVisit g v visitados = foldl (\acc u -> if u `elem` acc then acc else dfsVisit g u acc) (v : visitados) (g v)

dfs :: G -> [V]
dfs g = dfs' [start] []
  where
    start = head (vertices2 g)
    dfs' [] visitados = visitados
    dfs' (x:xs) visitados
      | x `elem` visitados = dfs' xs visitados
      | otherwise = dfs' (g x ++ xs) (x:visitados)

bfs :: G -> [V]
bfs g = bfs' [start] []
  where
    start = head (vertices2 g)
    bfs' [] visitados = visitados
    bfs' (x:xs) visitados
      | x `elem` visitados = bfs' xs visitados
      | otherwise = bfs' (xs ++ g x) (visitados ++ [x])

vertices3 :: Gn -> N -> [V]
vertices3 gn n = [v | v <- [0..n-1], not (null (gn (undefined, n) v))]

dijkstra :: V -> V -> GP -> Int
dijkstra o d gp = case lookup d (dijkstra' o gp) of
    Just costo -> costo
    Nothing -> error "No se encontró un camino"

dijkstra' :: V -> GP -> [(V, Int)]
dijkstra' o (gn, c) = dijkstraAux [(o, 0)] [] gn c
  where
    dijkstraAux [] costos _ _ = costos
    dijkstraAux ((v, costo):cola) costos gn c
      | v `elem` map fst costos = dijkstraAux cola costos gn c
      | otherwise = dijkstraAux nuevaCola ((v, costo) : costos) gn c
      where
        g = gn (undefined, 0)
        vecinos = g v
        nuevaCola = cola ++ [(u, costo + fromIntegral (c (v, u))) | u <- vecinos, u `notElem` map fst costos]

dijkstra'' :: V -> GP -> [(V, V)]
dijkstra'' o (gn, c) = dijkstraAux [(o, o)] [] gn c
  where
    dijkstraAux [] preds _ _ = preds
    dijkstraAux ((v, pred):cola) preds gn c
      | v `elem` map fst preds = dijkstraAux cola preds gn c
      | otherwise = dijkstraAux nuevaCola ((v, pred) : preds) gn c
      where
        g = gn (undefined, 0)
        vecinos = g v
        nuevaCola = cola ++ [(u, v) | u <- vecinos, u `notElem` map fst preds]

dijkstra''' :: V -> GP -> [(V, V, Int)]
dijkstra''' o gp = [(v, p, costo) | (v, costo) <- costos, (p, v') <- preds, v == v']
  where
    costos = dijkstra' o gp
    preds = dijkstra'' o gp

--Dijsktra visto en clase
dijkstraClase :: (A -> N, V -> Conj V) -> V -> V -> N
dijkstraClase gp o = dijkstraClase' gp o (visit o) (costos o)

dijkstraClase' :: (A -> N, V -> Conj V) -> V -> (V -> Bool) -> (V -> N) -> (V -> N)
dijkstraClase' (c, g) v vs cs
  | all vs (vertices2 g) = cs
  | otherwise = dijkstraClase' (c, g) (verticeMenorCosto g vs cs) (visitV v vs) (costosA c v (g v) cs)

costos :: V -> (V -> N)
costos o v = if v == o then 0 else 99999 -- número muy grande representa el infinito

visit :: V -> (V -> Bool)
visit _ _ = False

visitV :: V -> (V -> Bool) -> (V -> Bool)
visitV v vs v' = v == v' || vs v'

costosA :: (A -> N) -> V -> [V] -> (V -> N) -> (V -> N)
costosA c v [] cs = cs
costosA c v (a:ady) cs = \a' -> case a == a' of
  False -> cs a'
  True -> min (cs a') (cs v + c (v, a))
          where
            updatedCs = costosA c v ady cs


verticeMenorCosto :: (V -> Conj V) -> (V -> Bool) -> (V -> N) -> V
verticeMenorCosto g vs cs = head [u | u <- vertices2 g, not (vs u), cs u == minimum [cs x | x <- vertices2 g, not (vs x)]]

-- Función para obtener todas las aristas del grafo con sus pesos
obtenerAristasConPeso :: (A -> N) -> G -> [AP]
obtenerAristasConPeso c g = [(v1, v2, c (v1, v2)) | v1 <- vertices2 g, v2 <- g v1]

-- Función para ordenar las aristas por peso
ordenarAristas :: [AP] -> [AP]
ordenarAristas = sortBy (\(_, _, p1) (_, _, p2) -> compare p1 p2)

-- Funciones para manejar conjuntos disjuntos (Union-Find, MF-Set)
encontrar :: V -> [(V, V)] -> V
encontrar v padre = if v == fromJust (lookup v padre) then v else encontrar (fromJust (lookup v padre)) padre

unir :: V -> V -> [(V, V)] -> [(V, V)]
unir v1 v2 padre = (v1, raiz2) : padre
  where
    raiz1 = encontrar v1 padre
    raiz2 = encontrar v2 padre

kruskalAux :: [AP] -> [A] -> [(V, V)] -> [A]
kruskalAux [] mst _ = mst
kruskalAux ((v1, v2, _):as) mst padre
  | encontrar v1 padre /= encontrar v2 padre = kruskalAux as ((v1, v2) : mst) (unir v1 v2 padre)
  | otherwise = kruskalAux as mst padre

-- Función para construir el grafo a partir de las aristas
construirGrafo :: [A] -> G
construirGrafo aristas v = [u | (x, u) <- aristas, x == v] ++ [x | (x, u) <- aristas, u == v]

kruskal :: (A -> N) -> G -> G
kruskal c g = construirGrafo (kruskalAux aristasOrdenadas [] (map (\v -> (v, v)) vs))
  where
    aristas = obtenerAristasConPeso c g
    aristasOrdenadas = ordenarAristas aristas
    vs = vertices2 g
