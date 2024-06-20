--Grafos
type N = Integer --consideraremos solo a los naturales.
type Conj a = [a]
type V = N
type G = V -> Conj V
type Gn = (G,N) -> G
type A = (V,V)

--Ejemplos mencionados anteriormente
gid :: G
gid v = [v]
--Probar gid
--print (gid 5) -- Debería devolver [5]

-- Probar gleq
--print (gleq 3) -- Debería devolver una lista de números naturales menores o iguales a 3 (puede ser infinita, así que cuidado)

-- Probar gs
--print (gs 4) -- Debería devolver [5]

-- Probar geven
--print (geven 4) -- Debería devolver una lista de números naturales pares (puede ser infinita, así que cuidado)

-- Obtener los primeros 10 vértices del grafo geven
--print (take 10 (vertices geven)) -- Debería devolver los primeros 10 naturales pares

--Necesaria para obtener los v´ertices del grafo
vertices :: G -> Conj V
vertices g = [0..]

gleq :: G
gleq v = [x | x <- vertices gleq, x <= v]

gs :: G
gs v = [v+1]

geven :: G
geven v = [x | x <- vertices geven, even x]
--Probar geven
-- print (take 10 (geven 4)) -- debería imprimir [0,2,4,6,8,10,12,14,16,18]
-- print (gs 4) -- debería imprimir [5]
-- print (gleq 3) -- debería imprimir [0,1,2,3]
-- print (gid 5) -- debería imprimir [5]


geo :: G
geo v
    | even v = [e | even v, e <- vertices geo]
    | otherwise = [x | x <- vertices geo, odd x]
--Probar geo
-- print (take 10 (geo 4)) -- Debería devolver [0,2,4,6,8,10,12,14,16,18]
-- print (take 10 (geo 5)) -- Debería devolver [1,3,5,7,9,11,13,15,17,19]
-- print (take 10 (geo 5)) -- Debería devolver los primeros 10 números naturales impares
-- print (take 10 (geo 4)) -- Debería devolver los primeros 10 números naturales pares


gfin :: G
gfin 0 = [1]
gfin 1 = []
gfin 2 = [3,4]
gfin 3 = [2]
gfin 4 = [4,5]
gfin 5 = [2,3,5]
gfin _ = undefined

-- Probar gfin con varios valores
--print (gfin 0)  -- Debería devolver [1]
--print (gfin 1)  -- Debería devolver []
--print (gfin 2)  -- Debería devolver [3,4]
--print (gfin 3)  -- Debería devolver [2]
--print (gfin 4)  -- Debería devolver [4,5]
--print (gfin 5)  -- Debería devolver [2,3,5]

gn :: Gn
gn (g,n) v = if v < n then g v else undefined

geo2 :: G
geo2 v
    | even v = [e | even v, e <- vertices geo2]
    | otherwise = [x | x <- vertices geo2, odd x]

geo5 :: G
geo5 = gn (geo, 5)



existeArista :: A -> G -> Bool
existeArista = undefined

tieneLazoV :: V -> G -> Bool
tieneLazoV = undefined

esAislado :: V -> G -> Bool
esAislado = undefined

existeSimetrica :: A -> G -> Bool
existeSimetrica = undefined

gradoEntrada :: V -> G -> Bool
gradoEntrada = undefined

gradoSalida :: V -> G -> Bool
gradoSalida = undefined

gradoV :: V -> G -> Bool
gradoV = undefined

vertices2 :: G -> [V]
vertices2 = undefined

aristas :: G -> [A]
aristas = undefined

grado :: G -> Bool
grado = undefined

listaDeAdyacencia :: G -> [(V,[V])]
listaDeAdyacencia = undefined

matrizDeAdyacencia :: G -> [[Bool]]
matrizDeAdyacencia = undefined

parVA :: G -> ([V],[A])
parVA = undefined

agregarA :: A -> G -> G
agregarA = undefined

sacarA :: A -> G -> G
sacarA = undefined

esSubgrafo :: G -> G -> Bool
esSubgrafo = undefined

sonComplementarios :: G -> G -> Bool
sonComplementarios = undefined

complementario :: G -> G
complementario = undefined

esCamino :: [V] -> G -> [A]
esCamino = undefined

clausuraSimetrica :: G -> G
clausuraSimetrica = undefined

clausuraTransitiva :: G -> G
clausuraTransitiva = undefined

esCiclico :: G -> Bool
esCiclico = undefined

esConexo :: G -> Bool
esConexo = undefined

ordenTopologico :: G -> [V]
ordenTopologico = undefined

dfs :: G -> [V]
dfs = undefined

bfs :: G -> [V]
bfs = undefined

type GP = Integer -- Grafo ponderado, falta definir tipo
type C = Integer -- Costo, falta definir tipo

dijkstra :: V -> V -> GP -> Int
dijkstra = undefined

dijkstra' :: V -> GP -> [(V,Int)]
dijkstra' = undefined

dijkstra'' :: V -> GP -> [(V,V)]
dijkstra'' = undefined

dijkstra''' :: V -> GP -> [(V,V,C)]
dijkstra''' = undefined

kruskal :: G -> [V] -- debería devolver un grafo
kruskal = undefined