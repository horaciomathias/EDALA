module GAF2 where
--Grafos
type N = Integer --consideraremos solo a los naturales.
type Conj a = [a]
type V = N
type G = V -> Conj V
type Gn = (G,N)
type A = (V,V)
type AP = (V, V, N) -- esto o la aplicacion de la funcion de costo con cada arista
type C = A -> N
type GP = V -> C -> Conj V


geo :: G
geo v
    | even v = [e | e <- vertices geo, even e]
    | otherwise = [x | x <- vertices geo, odd x]

gleq :: G
gleq v = [x | x <- vertices gleq, x < v]

gLt :: G
gLt v = if v < 10 then [x | x <- take (fromIntegral v) (gleq v), x < v] else undefined

gn :: G
gn v = if v < 10 then [x | x <- take 10 (geo v), x < 10] else undefined

gCompletoSinLazos :: G
gCompletoSinLazos = complementario (gSoloLazo, 10)

gSoloLazo :: G
gSoloLazo v = if v < 10 then [x | x <- take 10 (geo v), x < 10, x == v] else undefined

gSinLazo :: G
gSinLazo v = if v < 10 then [x | x <- take 10 (geo v), x < 10, x /= v] else undefined

gAislado :: G
gAislado v = []

vertices :: G -> Conj V
vertices g = [0..]

fc :: C
fc (v1, v2) = if v1 == v2 then 0 else 2

-- grafo ponderado de prueba
grafoP1 :: GP
grafoP1 v fnC = gn v

existeArista :: A -> G -> Bool
existeArista (v1, v2) g = v2 `elem` g v1

tieneLazoV :: V -> G -> Bool
tieneLazoV v g = v `elem` g v

esAislado :: V -> G -> Bool
esAislado v g = null (g v)

existeSimetrica :: A -> G -> Bool
existeSimetrica (v1, v2) g = v2 `elem` g v1 && v1 `elem` g v2

incidentes :: V -> [V] -> G -> N
incidentes v [] g =  0
incidentes v [v1] g = if v `elem` g v1 then 1 else 0
incidentes v (v1 : vs) g = if v `elem` g v1 then 1 + incidentes v vs g else incidentes v vs g

gradoEntrada :: V -> Gn -> N
gradoEntrada v gn@(g, n) = incidentes v (vertices2 gn) g

vertices2 :: Gn -> [V]
vertices2 (g, n) = take (fromIntegral n) (vertices g)

gradoSalida :: V -> G -> N
gradoSalida v g = fromIntegral (length (g v))

gradoV :: V -> Gn -> N
gradoV v gn@(g,n) = gradoEntrada v gn + gradoSalida v g

aristas :: Gn -> [A]
aristas gn@(g,n) = [(v1, v2) | v1 <- vertices2 gn, v2 <- g v1]

grado :: Gn -> Bool
grado gn@(g,n) = all (\v -> gradoEntrada v gn == gradoSalida v g) (vertices2 gn)

listaDeAdyacencia :: Gn -> [(V,[V])]
listaDeAdyacencia gn@(g,n) = [(v, g v) | v <- vertices2 gn, not (null (g v))]

matrizDeAdyacencia :: Gn -> [[Bool]]
matrizDeAdyacencia gn@(g,n) = [[v2 `elem` g v1 | v2 <- vertices] | v1 <- vertices]
  where vertices = vertices2 gn

parVA :: Gn -> ([V],[A])
parVA gn@(g,n) = (vs, as)
  where
    vs = vertices2 gn
    as = aristas gn

agregarA :: A -> G -> G
agregarA (v1, v2) g v = if v == v1 then v2 : g v else g v

sacarA :: A -> G -> G
sacarA (v1, v2) g v = if v == v1 then filter (/= v2) (g v) else g v


esSubgrafo :: Gn -> Gn -> Bool
esSubgrafo gn1@(g1,n) gn2@(g2, m) = (m >= n) && incluida aristasG1 aristasG2
  where
    aristasG1 = aristas gn1
    aristasG2 = aristas gn2

incluida :: [A] -> [A] -> Bool
incluida [] _ = True
incluida (a:as) bs = a `elem` bs && incluida as bs

sonComplementarios :: Gn -> Gn -> Bool
sonComplementarios gn1@(g1,n) gn2@(g2, m) = (m == n) &&
    incluida aristasG1 aristasComplemento && incluida aristasComplemento aristasG1
        where
            aristasG1 = aristas gn1
            aristasComplemento = aristas (complementario gn2, m)

complementario :: Gn -> G
complementario gn@(g,n) = construirGrafo [(v1, v2) | v1 <- vertices2 gn, v2 <- vertices2 gn, not (existeArista (v1, v2) g)]

construirGrafo :: [A] -> G
construirGrafo aristas v = [v2 | (v1, v2) <- aristas, v1 == v]

esCamino :: [V] -> G -> [A]
esCamino [] _ = []
esCamino [v] _ = []
esCamino (v1:v2:vs) g
  | v2 `elem` g v1 = (v1, v2) : esCamino (v2:vs) g
  | otherwise = []

clausuraSimetrica :: Gn -> G
clausuraSimetrica gn@(g,n) v = g v ++ [u | u <- vertices2 gn, v `elem` g u, u `notElem` g v]


clausuraTransitiva :: Gn -> G
clausuraTransitiva gn@(g, n) v = [u | u <- vertices2 gn, hayCamino g v u]

hayCamino :: G -> V -> V -> Bool
hayCamino g v u
    | v == u = not (null (g v)) -- Aseguramos que exista una arista desde v, retornaba true para cualquier v del grafo gAislado porque dfs considera el vertice inicial como visitado
    | otherwise = u `elem` dfs g v

esCiclico :: Gn -> Bool
esCiclico gn@(g,n) = any (\v -> hayCamino g v v) (vertices2 gn)

esConexo :: Gn -> Bool
esConexo gn@(g, n) = all (`elem` visitados) (vertices2 gn)
  where
    visitados = dfs g (head (vertices2 gn))

ordenTopologico :: G -> [V]
ordenTopologico = undefined

dfs :: G -> V -> [V]
dfs g v = dfs' g [v] []

dfs' :: G -> [V] -> [V] -> [V]
dfs' _ [] visitados = visitados
dfs' g (x:xs) visitados
    | x `elem` visitados = dfs' g xs visitados
    | otherwise = dfs' g (g x ++ xs) (x:visitados)

bfs :: G -> V -> [V]
bfs g v = bfs' g [v] []

bfs' :: G -> [V] -> [V] -> [V]
bfs' _ [] visitados = visitados
bfs' g (x:xs) visitados
    | x `elem` visitados = bfs' g xs visitados
    | otherwise = bfs' g (xs ++ g x) (visitados ++ [x])


--Dijsktra visto en clase
dijkstraClase :: (C, Gn) -> V -> V -> N
dijkstraClase (c, gn@(g,n)) o = dijkstraClase' (c, gn) o (visit o) (costos o)

dijkstraClase' :: (C, Gn) -> V -> (V -> Bool) -> (V -> N) -> V -> N
dijkstraClase' (c, gn@(g, n)) v vs cs d
  | all vs (vertices2 gn) = cs d
  | otherwise = dijkstraClase' (c, gn) (verticeMenorCosto gn vs cs) (visitV v vs) (costosA c v (g v) cs) d

costos :: V -> (V -> N)
costos o v = if v == o then 0 else 99999 -- número muy grande, representa el infinito

visit :: V -> (V -> Bool)
visit _ _ = False

visitV :: V -> (V -> Bool) -> (V -> Bool)
visitV v vs v' = v == v' || vs v'

costosA :: (A -> N) -> V -> [V] -> (V -> N) -> (V -> N)
costosA c v [] cs = cs
costosA c v (a:ady) cs = costosA c v ady updatedCs
  where
    updatedCs a' = if a == a'
                       then min (cs a') (cs v + c (v, a))
                       else cs a'

verticeMenorCosto :: Gn -> (V -> Bool) -> (V -> N) -> V
verticeMenorCosto gn@(g,n) vs cs = foldr1 (\u v -> if cs u < cs v then u else v) [u | u <- vertices2 gn, not (vs u)]


kruskal :: G -> [V]
kruskal = undefined

--Auxiliares
estaIncluida :: Eq a => [a] -> [a] -> Bool
estaIncluida [] _ = True
estaIncluida (a:as) bs = a `elem` bs && estaIncluida as bs

mismosElementos :: Eq a => [a] -> [a] -> Bool
mismosElementos as bs = estaIncluida as bs && estaIncluida bs as
