module GraphsAsFunctions where
import Data.List
import Data.Maybe

-- Grafos
type N = Integer -- Consideraremos solo a los naturales.
type Conj a = [a]
type V = N
type G = V -> Conj V
type Gn = (G,N)
type A = (V,V)
type AP = (V, V, N) -- Esto o la aplicacion de la funcion de costo con cada arista
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

--Definir una funcion de costo
c :: C
c (v1, v2) = if v1 == v2 then 0 else 2

geo2 :: G
geo2 v
    | even v = [e | even v, e <- vertices geo2]
    | otherwise = [x | x <- vertices geo2, odd x]

existeArista :: A -> Gn -> Bool
existeArista (v1, v2) (g, n) = v2 `elem` g v1

tieneLazoV :: V -> Gn -> Bool
tieneLazoV v (g, n) = v `elem` g v

esAislado :: V -> Gn -> Bool
esAislado v (g, n) = null (g v)

existeSimetrica :: A -> Gn -> Bool
existeSimetrica (v1, v2) (g, n) = v2 `elem` g v1 && v1 `elem` g v2

gradoEntrada :: V -> GP -> Bool
gradoEntrada v ((g, n), c) = length [x | x <- vertices g, existeArista (x, v) (g, n)] == length [x | x <- vertices g, existeArista (v, x) (g, n)]

gradoSalida :: V -> GP -> Bool
gradoSalida v ((g, n), c) = length [x | x <- vertices g, existeArista (v, x) (g, n)] == length [x | x <- vertices g, existeArista (x, v) (g, n)]

gradoV :: V -> GP -> Bool
gradoV v gp = gradoEntrada v gp && gradoSalida v gp

vertices2 :: Gn -> [V]
vertices2 (g, n) = [0..n]

aristas :: Gn -> [A]
aristas (g, n) = [(v1, v2) | v1 <- vertices2 (g, n), v2 <- g v1]

grado :: Gn -> Bool
grado (g, n) = all (\v -> gradoV v ((g, n), c)) (vertices2 (g, n))

listaDeAdyacencia :: Gn -> [(V,[V])]
listaDeAdyacencia (g, n) = [(v, g v) | v <- vertices2 (g, n)]

matrizDeAdyacencia :: Gn -> [[Bool]]
matrizDeAdyacencia (g, n) = [[existeArista (v1, v2) (g, n) | v2 <- vertices2 (g, n)] | v1 <- vertices2 (g, n)]

parVA :: GP -> ([V],[AP])
parVA ((g, n), c) = (vertices2 (g, n), [(v1, v2, c (v1, v2)) | v1 <- vertices2 (g, n), v2 <- g v1])

agregarA :: AP -> GP -> GP
agregarA (v1, v2, p) ((g, n), c) =
    let g' v = if v == v1 then v2 : g v else g v
        c' a@(x, y) = if a == (v1, v2) then p else c a
    in ((g', n), c')

sacarA :: AP -> GP -> GP
sacarA (v1, v2, p) ((g, n), c) =
    let g' v = if v == v1 then filter (/= v2) (g v) else g v
        c' a@(x, y) = if a == (v1, v2) then 0 else c a
    in ((g', n), c')

esSubgrafo :: GP -> GP -> Bool
esSubgrafo ((g1, n1), c1) ((g2, n2), c2) = all (\v -> g1 v == g2 v) (vertices2 (g1, n1))

sonComplementarios :: GP -> GP -> Bool
sonComplementarios gp1 gp2 = esSubgrafo gp1 (complementario gp2) && esSubgrafo gp2 (complementario gp1)

complementario :: GP -> GP
complementario ((g, n), c) =
    let g' v = filter (\x -> not (existeArista (v, x) (g, n))) (vertices2 (g, n))
        c' a = if existeArista a (g, n) then 0 else 1
    in ((g', n), c')

esCamino :: [V] -> GP -> [AP]
esCamino [] _ = []
esCamino [x] _ = []
esCamino (x:y:xs) gp = if existeArista (x, y) (fst gp) then (x, y, snd gp (x, y)) : esCamino (y:xs) gp else []

clausuraSimetrica :: GP -> GP
clausuraSimetrica ((g, n), c) =
    let g' v = nub (g v ++ [x | x <- vertices2 (g, n), v `elem` g x])
        c' (u, v)
          | existeArista (u, v) (g, n) = c (u, v)
          | existeArista (v, u) (g, n) = c (v, u)
          | otherwise = 0
    in ((g', n), c')


clausuraTransitiva :: GP -> GP
clausuraTransitiva ((g, n), c) =
    let allVertices = vertices2 (g, n)
        alcanzables v = nub (alcanzables' [v] [])
        alcanzables' [] visitados = visitados
        alcanzables' (x:xs) visitados
            | x `elem` visitados = alcanzables' xs visitados
            | otherwise = alcanzables' (xs ++ g x) (x : visitados)
        g' v = filter (/= v) (alcanzables v) -- Elimina el mismo vértice para evitar lazo
        c' (u, v)
          | u == v = 0
          | existeArista (u, v) (g, n) = c (u, v)
          | otherwise = 1
    in ((g', n), c')

esCiclico :: GP -> Bool
esCiclico ((g, n), c) = any (\v -> v `elem` g v) (vertices2 (g, n))

esConexo :: GP -> Bool
esConexo ((g, n), c) = all (\v -> not (esAislado v (g, n))) (vertices2 (g, n))

ordenTopologico :: GP -> [V]
ordenTopologico ((g, n), c) = ordenTopologico' (vertices2 (g, n)) (const False) []
  where
    ordenTopologico' [] visitados resultado = reverse resultado
    ordenTopologico' (v:vs) visitados resultado
      | visitados v = ordenTopologico' vs visitados resultado
      | all visitados (g v) = ordenTopologico' vs (\x -> x == v || visitados x) (v : resultado)
      | otherwise = ordenTopologico' (vs ++ [v]) visitados resultado

dfs :: GP -> [V]
dfs gp@(gn@(g, n), c) = dfs' (vertices2 gn) (const False)

dfs' :: [a] -> (a -> Bool) -> [a]
dfs' [] visitados = []
dfs' (v:vs) visitados
  | visitados v = dfs' vs visitados
  | otherwise = v : dfs' (g v ++ vs) (\x -> x == v || visitados x)

bfs :: GP -> [V]
bfs ((g, n), c) = bfs' (vertices2 (g, n)) (const False) g

bfs' :: [V] -> (V -> Bool) -> G -> [V]
bfs' [] visitados _ = []
bfs' (v:vs) visitados g
  | visitados v = bfs' vs visitados g
  | otherwise = v : bfs' (vs ++ g v) (\x -> x == v || visitados x) g

infinito :: Int
infinito = 999999

dijkstra :: V -> V -> GP -> Int
dijkstra origen destino gp = case lookup destino (dijkstra' origen gp) of
    Just distancia -> distancia
    Nothing -> infinito

dijkstra' :: V -> GP -> [(V, Int)]
dijkstra' origen gp@((g, n), c) = dijkstraRec [origen] [] (inicializarDistancias (vertices2 (g, n)) origen)
  where
    inicializarDistancias :: [V] -> V -> [(V, Int)]
    inicializarDistancias vs origen = [(v, if v == origen then 0 else infinito) | v <- vs]

    dijkstraRec :: [V] -> [V] -> [(V, Int)] -> [(V, Int)]
    dijkstraRec [] _ distancias = distancias
    dijkstraRec (v:vs) visitados distancias =
        let vecinos = g v
            distV = fromJust (lookup v distancias)
            distancias' = foldl (actualizarDistancia v distV) distancias vecinos
            visitados' = v : visitados
            (v', _) = minimoNoVisitado distancias' visitados'
        in dijkstraRec (delete v' (vertices2 (g, n)) ++ vs) visitados' distancias'

    actualizarDistancia :: V -> Int -> [(V, Int)] -> V -> [(V, Int)]
    actualizarDistancia v distV distancias vecino =
        let alt = distV + fromIntegral (c (v, vecino))
            distVecino = fromJust (lookup vecino distancias)
        in if alt < distVecino
           then (vecino, alt) : delete (vecino, distVecino) distancias
           else distancias

    minimoNoVisitado :: [(V, Int)] -> [V] -> (V, Int)
    minimoNoVisitado distancias visitados =
        let noVisitados = filter (\(v, _) -> v `notElem` visitados) distancias
        in foldl1 (\acc x -> if snd x < snd acc then x else acc) noVisitados

dijkstra'' :: V -> GP -> [(V, V)]
dijkstra'' origen gp@((g, n), c) = dijkstraRec [origen] [] (inicializarDistancias (vertices2 (g, n)) origen) []
  where
    inicializarDistancias :: [V] -> V -> [(V, Int)]
    inicializarDistancias vs origen = [(v, if v == origen then 0 else infinito) | v <- vs]

    dijkstraRec :: [V] -> [V] -> [(V, Int)] -> [(V, V)] -> [(V, V)]
    dijkstraRec [] _ _ caminos = caminos
    dijkstraRec (v:vs) visitados distancias caminos =
        let vecinos = g v
            distV = fromJust (lookup v distancias)
            (distancias', caminos') = foldl (actualizarDistanciaYCaminos v distV) (distancias, caminos) vecinos
            visitados' = v : visitados
            (v', _) = minimoNoVisitado distancias' visitados'
        in dijkstraRec (delete v' (vertices2 (g, n)) ++ vs) visitados' distancias' caminos'

    actualizarDistanciaYCaminos :: V -> Int -> ([(V, Int)], [(V, V)]) -> V -> ([(V, Int)], [(V, V)])
    actualizarDistanciaYCaminos v distV (distancias, caminos) vecino =
        let alt = distV + fromIntegral (c (v, vecino))
            distVecino = fromJust (lookup vecino distancias)
        in if alt < distVecino
           then ((vecino, alt) : delete (vecino, distVecino) distancias, (vecino, v) : caminos)
           else (distancias, caminos)

    minimoNoVisitado :: [(V, Int)] -> [V] -> (V, Int)
    minimoNoVisitado distancias visitados =
        let noVisitados = filter (\(v, _) -> v `notElem` visitados) distancias
        in foldl1 (\acc x -> if snd x < snd acc then x else acc) noVisitados

dijkstra''' :: V -> GP -> [(V, V, N)]
dijkstra''' origen gp@((g, n), c) = [(v, u, c (u, v)) | (v, u) <- dijkstra'' origen gp]


--Dijsktra visto en clase
dijkstraClase :: GP -> V -> V -> N
dijkstraClase gp@((g, n), cs) o = dijkstraClase' gp o (const False) (costos o)  

dijkstraClase' :: GP -> V -> (V -> Bool) -> (V -> N) -> (V -> N)
dijkstraClase' gp@(gn@(g, n), fc) v fVisitados fCostos
  | all fVisitados (vertices2 gn) = fCostos
  | otherwise = dijkstraClase' gp (verticeMenorCosto gp fVisitados fCostos) (visitV v fVisitados) (costosA c v (g v) fCostos)

costos :: V -> (V -> N)
costos o v = if v == o then 0 else 99999 -- número muy grande, representa el infinito

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


verticeMenorCosto :: GP -> (V -> Bool) -> (V -> N) -> V
verticeMenorCosto ((g, n), c) vs cs = head [u | u <- vertices2 g, not (vs u), cs u == minimum [cs x | x <- vertices2 g, not (vs x)]]

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
kruskalAux [] acm _ = acm
kruskalAux ((v1, v2, _):as) acm padre
  | encontrar v1 padre /= encontrar v2 padre = kruskalAux as ((v1, v2) : acm) (unir v1 v2 padre)
  | otherwise = kruskalAux as acm padre


kruskal :: GP -> GP
kruskal ((g, n), c) = ((g', n), c)
  where
    edges = ordenarAristas c [(u, v) | u <- vertices2 (g, n), v <- g u]
    vertices2 (g, n) = [0..n-1]
    g' v = [u | (u, v') <- acm, v' == v] ++ [v' | (u, v') <- acm, u == v]
    
    acm = kruskal' edges []

    kruskal' [] acm = acm
    kruskal' ((u, v):es) acm
      | not (connected u v acm) = kruskal' es ((u, v) : acm)
      | otherwise = kruskal' es acm
    
    connected u v acm = find u acm == find v acm

    find v acm = find' v (map (\(x, y) -> (y, x)) acm ++ acm)

    find' v [] = v
    find' v ((x, y):xs)
      | v == x = find' y xs
      | otherwise = find' v xs

    --QickSort
    ordenarAristas c [] = []
    ordenarAristas c (pivote:resto) =
      ordenarAristas c [y | y <- resto, c y <= c pivote]
      ++ [pivote] ++
      ordenarAristas c [y | y <- resto, c y > c pivote]