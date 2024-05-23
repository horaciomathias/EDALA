module Grafos where
--Elegimos una representacion del grafo como una lista de adyacencia (mas facil de recorrer en haskell)

type Conj a = [a] --Esta bueno para hacer operaciones de conjuntos
type V = Integer
type A = (V,V)


--distintos tipos de representaciones de grafos
--type G = (Conj V, Conj A) -- Para usar con conjuntos
--type G = [(V,Conj (V, Cost))] -- Esto ya admite multicosto, no precisa otra cosa para multigrafos.
type G = Conj (V, Conj V) -- Esto es un grafo con lista de adyacencia, pero implementado con conjuntos. Acá no puede haber repetidos.

g :: G
g = [
    (1, [3, 4]),
    (2, [3, 4]),
    (3, [1, 2]),
    (4, [1, 2, 4]),
    (5, [5])
    ]

gSinLazos :: G
gSinLazos = [
    (1, [2, 3]),
    (2, [1, 3]),
    (3, [1, 2])
    ]

gConLazos :: G
gConLazos = [
    (1, [1, 2, 3]),
    (2, [2, 3]),
    (3, [3])
    ]

--Funciones basicas de conjuntos
union :: Eq a => Conj a -> Conj a -> Conj a
union  xs ys = xs ++ [y | y <- ys, y `notElem` xs]

intersection :: Eq a => Conj a -> Conj a -> Conj a
intersection xs ys = [x | x <- xs, x `elem` ys]

difference  :: Eq a => Conj a -> Conj a -> Conj a
difference xs ys = [x | x <- xs, x `notElem` ys]

inSet :: Eq a => a -> Conj a -> Bool
inSet x xs = x `elem` xs

esDirigido :: G -> Bool --Existe alguna arista (v1, v2), no existe (v2, v1)
esDirigido g = esDirigidoAux g (aristas g)

esDirigidoAux :: G -> [A] -> Bool
esDirigidoAux [] [] = True
esDirigidoAux [] ag = False
esDirigidoAux ((v,[]):g') ag = esDirigidoAux g' ag
esDirigidoAux ((o, d : ds):g') ag
 | (d, o) `notElem` ag = True
 | otherwise = esDirigidoAux ((o,ds):g') ag

tieneLazos :: G -> Bool
tieneLazos [] = False
tieneLazos ((v,a):g) = elem v a || tieneLazos g

--si son de vertices pueden salir facil recorriendo el grafo
--si son de aristas hay que tener algo donde se recorran las aristas pero no perderlas porque podemos olvidar cosas (algo del estilo de esDirigido)

esLazo :: V -> G -> Bool
esLazo v [] = False
esLazo v g =  (v, v) `elem` aristas g

tieneAislados :: G -> Bool
tieneAislados [] = False
tieneAislados ((v,a): g) = v `notElem` a || tieneAislados g

esAislado :: V -> G -> Bool
esAislado v g = gE v g == 0 && gS v g == 0 

gE :: V -> G -> Integer
gE v g = fromIntegral $ length $ incidentes v g 

gS :: V -> G -> Integer
gS v g = fromIntegral $ length $ adyacentes v g

gV :: V -> G -> Integer
gV v g = gE v g + gS v g 

agregarArista :: V -> V -> G -> G
agregarArista v v' [] = [(v, [v'])]
agregarArista v v' ((o, ds):g) 
 | o == v = (o, ds `union` [v']) : g
 | otherwise = (o, ds) : agregarArista v v' g

aristas :: G -> [A]
aristas [] = []
aristas ((v,[]):vas) = aristas vas --solo acá achica el grafo
aristas ((o,d:as):vas) = (o,d):aristas ((o,as):vas)--doble recursión, acá se achica la lista de adyacencia


simetricas :: A -> A
simetricas (o, d) = (d, o)

vertices :: G -> [V]
vertices [] = []
vertices ((v, _):g) = v : vertices g

adyacentes :: V -> G -> [V]
adyacentes v [] = []
adyacentes v ((o, ds):g)
 | v == o = ds
 | otherwise = adyacentes v g

incidentes :: V -> G -> [V]
incidentes v [] = []
incidentes v ((o, ds) : g) 
    | v `elem` ds = o : incidentes v g
    | otherwise = incidentes v g 

-- debería crear un grafo completo a partir de una lista de vertices?
completo:: [V] -> G
completo [] = []
completo (v:vs) = completoAux (v:vs) (v:vs)

completoAux :: [V] -> [V] -> G
completoAux [] vs = []
completoAux (v:vs) vs' = (v, vs') : completoAux vs vs'

esCompleto :: G -> Bool
esCompleto [] = True
esCompleto g = esCompletoAux (vertices g) g

esCompletoAux :: [V] -> G -> Bool
esCompletoAux vs g = foldr (\ v -> (&&) (length (adyacentes v g) == length (vertices g))) True vs


subgrafos :: G -> [G]
subgrafos = undefined

esSubgrafo :: G -> G -> Bool
esSubgrafo [] _ = True
esSubgrafo g [] = False
esSubgrafo g1 g2 = aristas g1 `intersection` aristas g2 == aristas g1 && vertices g1 `intersection` vertices g2 == vertices g1

complemento :: G -> G
complemento [] = []
complemento g = complementoAux (vertices g) g (completo (vertices g)) 

-- no anda
complementoAux :: [V] -> G -> G -> G
complementoAux vs g completo = map (\ v -> (v, difference (adyacentes v completo) (vertices g))) vs


esComplemento :: G -> G -> Bool
esComplemento g1 g2 = complemento g1 == g2
