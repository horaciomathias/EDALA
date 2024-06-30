import GraphsAsFunctions
import Control.Monad.RWS (MonadState(put))
import Distribution.PackageDescription (KnownRepoType(GnuArch))

main :: IO ()
main = do
    putStrLn "Pruebas de funciones de grafos:"

    -- Probar gid
    putStrLn $ "gid 5: " ++ show (gid 5 == [5])

    -- Probar gleq
    putStrLn $ "gleq 3: " ++ show (take 4 (gleq 3) == [0, 1, 2, 3])

    -- Probar gs
    putStrLn $ "gs 4: " ++ show (gs 4 == [5])

    -- Probar geven
    putStrLn $ "geven 4: " ++ show (take 5 (geven 4) == [0, 2, 4, 6, 8])

    -- Probar vertices con geven
    putStrLn $ "vertices geven: " ++ show (take 10 (vertices geven) == [0..9])

    -- Probar geo
    putStrLn $ "geo 4: " ++ show (take 10 (geo 4) == [0, 2, 4, 6, 8, 10, 12, 14, 16, 18])
    putStrLn $ "geo 5: " ++ show (take 10 (geo 5) == [1, 3, 5, 7, 9, 11, 13, 15, 17, 19])

    -- Probar gfin con varios valores
    putStrLn $ "gfin 0: " ++ show (gfin 0 == [1])
    putStrLn $ "gfin 1: " ++ show (null (gfin 1))
    putStrLn $ "gfin 2: " ++ show (gfin 2 == [3, 4])
    putStrLn $ "gfin 3: " ++ show (gfin 3 == [2])
    putStrLn $ "gfin 4: " ++ show (gfin 4 == [4, 5])
    putStrLn $ "gfin 5: " ++ show (gfin 5 == [2, 3, 5])

    -- Probar existeArista
    putStrLn $ "existeArista (0, 1) gfin: " ++ show (existeArista (0, 1) gfin)
    putStrLn $ "existeArista (1, 2) gfin: " ++ show (not (existeArista (1, 2) gfin))
    putStrLn $ "existeArista (2, 3) gfin: " ++ show (existeArista (2, 3) gfin)
    putStrLn $ "existeArista (4, 5) gfin: " ++ show (existeArista (4, 5) gfin)
    putStrLn $ "existeArista (5, 1) gfin: " ++ show (not (existeArista (5, 1) gfin))

    -- Probar tieneLazoV
    putStrLn $ "tieneLazoV 0 gfin: " ++ show (not (tieneLazoV 0 gfin))
    putStrLn $ "tieneLazoV 1 gfin: " ++ show (not (tieneLazoV 1 gfin))
    putStrLn $ "tieneLazoV 2 gfin: " ++ show (not (tieneLazoV 2 gfin))
    putStrLn $ "tieneLazoV 3 gfin: " ++ show (not (tieneLazoV 3 gfin))
    putStrLn $ "tieneLazoV 4 gfin: " ++ show (tieneLazoV 4 gfin)
    putStrLn $ "tieneLazoV 5 gfin: " ++ show (tieneLazoV 5 gfin)

   -- Probar con gid
    putStrLn $ "listaDeAdyacencia gid: " ++ show (take 5 (listaDeAdyacencia gid) == [(0,[0]), (1,[1]), (2,[2]), (3,[3]), (4,[4])])

    -- Probar con gleq
    --putStrLn $ "listaDeAdyacencia gleq: " ++ show (take 5 (listaDeAdyacencia gleq) == [(0,[0]), (1,[0,1]), (2,[0,1,2]), (3,[0,1,2,3]), (4,[0,1,2,3])])

    -- Probar con gs
    --putStrLn $ "listaDeAdyacencia gs: " ++ show (take 5 (listaDeAdyacencia gs) == [(0,[1]), (1,[2]), (2,[3]), (3,[4]), (4,[5])])

    -- Probar con geven
    --putStrLn $ "listaDeAdyacencia geven: " ++ show (take 5 (listaDeAdyacencia geven) == [(0,[0,2,4,6,8,10]), (1,[0,2,4,6,8,10]), (2,[0,2,4,6,8,10]), (3,[0,2,4,6,8,10]), (4,[0,2,4,6,8,10])])

    -- Probar con geo
    --putStrLn $ "listaDeAdyacencia geo (primeros 5 pares): " ++ show (take 5 (listaDeAdyacencia geo) == [(0,[0,2,4,6,8,10]), (1,[1,3,5,7,9,11]), (2,[0,2,4,6,8,10]), (3,[1,3,5,7,9,11]), (4,[0,2,4,6,8,10])])

    -- Probar con gfin
    --putStrLn $ "listaDeAdyacencia gfin: " ++ show (take 6 (listaDeAdyacencia gfin) == [(0,[1]), (1,[]), (2,[3,4]), (3,[2]), (4,[4,5]), (5,[2,3,5])])


    -- Probar esAislado
    putStrLn $ "esAislado 1 gfin: " ++ show (esAislado 1 gfin)
    putStrLn $ "esAislado 0 gfin: " ++ show (not (esAislado 0 gfin))

    -- Probar existeSimetrica
    putStrLn $ "existeSimetrica (0, 1) gfin: " ++ show (not (existeSimetrica (0, 1) gfin))
    putStrLn $ "existeSimetrica (4, 5) gfin: " ++ show (not (existeSimetrica (4, 5) gfin))
    putStrLn $ "existeSimetrica (5, 4) gfin: " ++ show (not (existeSimetrica (5, 4) gfin))

    -- Probar gradoEntrada, gradoSalida, gradoV - estas pruebas quedan en loop infinito
    --putStrLn $ "gradoEntrada 2 gfin: " ++ show (gradoEntrada 2 gfin == 2)
    --putStrLn $ "gradoSalida 2 gfin: " ++ show (gradoSalida 2 gfin == 2)
    --putStrLn $ "gradoV 2 gfin: " ++ show (gradoV 2 gfin == 4)


    -- Probar vertices2 - queda en loop
    --putStrLn $ "vertices2 gfin: " ++ show (vertices2 gfin == [0, 2, 3, 4, 5])

    -- Probar aristas - queda en loop
    --putStrLn $ "aristas gfin: " ++ show (aristas gfin == [(0, 1), (2, 3), (2, 4), (3, 2), (4, 4), (4, 5), (5, 2), (5, 3), (5, 5)])

    -- Probar grado - queda en loop
    --putStrLn $ "grado gfin: " ++ show (not (grado gfin))
    --putStrLn $ "grado gid: " ++ show (grado gid)

    -- Probar listaDeAdyacencia - queda en loop
    --putStrLn $ "listaDeAdyacencia gfin: " ++ show (listaDeAdyacencia gfin == [(0, [1]), (1, []), (2, [3, 4]), (3, [2]), (4, [4, 5]), (5, [2, 3, 5])])

    -- Probar matrizDeAdyacencia - hay que buscar otra alternativa, devuelve false 
    --putStrLn $ "matrizDeAdyacencia gfin: " ++ show (take 6 (matrizDeAdyacencia gfin) == [[False, True, False, False, False, False], [False, False, False, False, False, False], [False, False, False, True, True, False], [False, False, True, False, False, False], [False, False, False, False, True, True], [False, False, True, True, False, True]])

    -- Probar parVA - queda en loop
    --putStrLn $ "parVA gfin: " ++ show (parVA gfin == ([0, 2, 3, 4, 5], [(0, 1), (2, 3), (2, 4), (3, 2), (4, 4), (4, 5), (5, 2), (5, 3), (5, 5)]))

    -- Probar agregarA
    let gfin2 = agregarA (1, 0) gfin
    putStrLn $ "agregarA (1, 0) gfin: " ++ show (existeArista (1, 0) gfin2)

    -- Probar sacarA
    let gfin3 = sacarA (5, 2) gfin
    putStrLn $ "sacarA (5, 2) gfin: " ++ show (not (existeArista (5, 2) gfin3))

    -- Probar esSubgrafo
    putStrLn $ "No esSubgrafo gid gfin: " ++ show (not (esSubgrafo gid gfin))
    putStrLn $ "No esSubgrafo gfin gid: " ++ show (not (esSubgrafo gfin gid))

    -- Probar sonComplementarios
    putStrLn $ "No sonComplementarios gid gfin: " ++ show (not (sonComplementarios gid gfin))
    --putStrLn $ "sonComplementarios gfin (complementario gfin): " ++ show (sonComplementarios gfin (complementario gfin)) --queda en loop

    -- Probar esCamino
    putStrLn $ "esCamino [0, 1] gfin: " ++ show (esCamino [0, 1] gfin == [(0, 1)])
    putStrLn $ "esCamino [0, 2] gfin: " ++ show (null (esCamino [0, 2] gfin))

    -- Probar clausuraSimetrica
    let gsim = clausuraSimetrica gfin
    putStrLn $ "clausuraSimetrica gfin: " ++ show (existeSimetrica (0, 1) gsim && existeSimetrica (1, 0) gsim)

    -- Probar clausuraTransitiva
    let gtrans = clausuraTransitiva gfin
    putStrLn $ "clausuraTransitiva gfin: " ++ show (existeArista (2, 5) gtrans)

    -- Probar esCiclico
    putStrLn $ "esCiclico gfin: " ++ show (esCiclico gfin)
    putStrLn $ "esCiclico gid: " ++ show (esCiclico gid)

    -- Probar esConexo - queda en loop
    --putStrLn $ "esConexo gfin: " ++ show (not (esConexo gfin))
    --putStrLn $ "esConexo gid: " ++ show (esConexo gid)

    -- Probar ordenTopologico - queda en loop
    --putStrLn $ "ordenTopologico gfin: " ++ show (ordenTopologico gfin == [1, 0, 2, 3, 4, 5])

    -- Probar dfs - dificil de probar con un G
    --putStrLn $ "dfs gfin: " ++ show (dfs gfin == [0, 1, 2, 3, 4, 5])

    -- Probar bfs - dificil de probar con un G
    --putStrLn $ "bfs gfin: " ++ show (bfs gfin == [0, 1, 2, 3, 4, 5])

    -- Definir 2 vertices
    let v0 = 0
    let v1 = 1
    let v2 = 2
    let v3 = 3
    let v4 = 4
    let v5 = 5

    gp :: GP
    gp = (\(u, v) -> 1, \v -> if v <= 5 then gleq v else [])
    -- probar dijkstra con gp
    putStrLn $ "dijkstra gp 0 5: " ++ show (dijkstra v2 v1 gp)
    putStrLn $ "dijkstra gp 0 4: " ++ show (dijkstra v4 v3 gp)
    putStrLn $ "dijkstra gp 2 5: " ++ show (dijkstra v5 v0 gp)


    -- Probar dijkstraClase con diferentes grafos y costos
    let gfinCostClase1 = (\(u, v) -> 1, \v -> if v <= 5 then gfin v else [])
    let gfinCostClase2 = (\(u, v) -> if u == v then 0 else 1, \v -> if v <= 5 then gfin v else [])
    let gfinCostClase3 = (uncurry (+), \v -> if v <= 5 then gfin v else [])

    putStrLn $ "dijkstraClase gfinCostClase1 0 5: " ++ show (dijkstraClase gfinCostClase1 0 5 == 3)
    putStrLn $ "dijkstraClase gfinCostClase1 0 4: " ++ show (dijkstraClase gfinCostClase1 0 4 == 2)
    putStrLn $ "dijkstraClase gfinCostClase1 2 5: " ++ show (dijkstraClase gfinCostClase1 2 5 == 2)

    putStrLn $ "dijkstraClase gfinCostClase2 0 5: " ++ show (dijkstraClase gfinCostClase2 0 5 == 3)
    putStrLn $ "dijkstraClase gfinCostClase2 0 4: " ++ show (dijkstraClase gfinCostClase2 0 4 == 2)
    putStrLn $ "dijkstraClase gfinCostClase2 2 5: " ++ show (dijkstraClase gfinCostClase2 2 5 == 2)

    putStrLn $ "dijkstraClase gfinCostClase3 0 5: " ++ show (dijkstraClase gfinCostClase3 0 5 == 12)
    putStrLn $ "dijkstraClase gfinCostClase3 0 4: " ++ show (dijkstraClase gfinCostClase3 0 4 == 7)
    putStrLn $ "dijkstraClase gfinCostClase3 2 5: " ++ show (dijkstraClase gfinCostClase3 2 5 == 10)

    -- Probar kruskal con diferentes funciones de costo
    let gminSpanTree = kruskal (\(u, v) -> 1) gfin

    let gfinCost1 :: A -> N
        gfinCost1 = const 1
    let gfinCost2 :: A -> N
        gfinCost2 (u, v) = if u == v then 0 else 1
    let gfinCost3 :: A -> N
        gfinCost3 (u, v) = u + v

    let gminSpanTree1 = kruskal gfinCost1 gfin
    let gminSpanTree2 = kruskal gfinCost2 gfin
    let gminSpanTree3 = kruskal gfinCost3 gfin

    putStrLn $ "kruskal gfin: " ++ show (esConexo gminSpanTree && not (esCiclico gminSpanTree))
    putStrLn $ "kruskal gfin (cost1): " ++ show (esConexo gminSpanTree1 && not (esCiclico gminSpanTree1))
    putStrLn $ "kruskal gfin (cost2): " ++ show (esConexo gminSpanTree2 && not (esCiclico gminSpanTree2))
    putStrLn $ "kruskal gfin (cost3): " ++ show (esConexo gminSpanTree3 && not (esCiclico gminSpanTree3))

    -- Probar kruskal con otro grafo
    let g2 :: G
        g2 0 = [1, 2]
        g2 1 = [0, 3]
        g2 2 = [0, 3]
        g2 3 = [1, 2]
        g2 _ = []

    let g2Cost1 :: A -> N
        g2Cost1 = const 1
    let g2Cost2 :: A -> N
        g2Cost2 (u, v) = if u == v then 0 else 1
    let g2Cost3 :: A -> N
        g2Cost3 (u, v) = u + v

    let g2minSpanTree1 = kruskal g2Cost1 g2
    let g2minSpanTree2 = kruskal g2Cost2 g2
    let g2minSpanTree3 = kruskal g2Cost3 g2

    putStrLn $ "kruskal g2 (cost1): " ++ show (esConexo g2minSpanTree1 && not (esCiclico g2minSpanTree1))
    putStrLn $ "kruskal g2 (cost2): " ++ show (esConexo g2minSpanTree2 && not (esCiclico g2minSpanTree2))
    putStrLn $ "kruskal g2 (cost3): " ++ show (esConexo g2minSpanTree3 && not (esCiclico g2minSpanTree3))

    -- Probar kruskal con un grafo más grande
    let g3 :: G
        g3 0 = [1, 2, 3]
        g3 1 = [0, 2, 4]
        g3 2 = [0, 1, 4]
        g3 3 = [0, 4]
        g3 4 = [1, 2, 3]
        g3 _ = []

    let g3Cost1 :: A -> N
        g3Cost1 = const 1
    let g3Cost2 :: A -> N
        g3Cost2 (u, v) = if u == v then 0 else 1
    let g3Cost3 :: A -> N
        g3Cost3 (u, v) = u + v

    let g3minSpanTree1 = kruskal g3Cost1 g3
    let g3minSpanTree2 = kruskal g3Cost2 g3
    let g3minSpanTree3 = kruskal g3Cost3 g3

    putStrLn $ "kruskal g3 (cost1): " ++ show (esConexo g3minSpanTree1 && not (esCiclico g3minSpanTree1))
    putStrLn $ "kruskal g3 (cost2): " ++ show (esConexo g3minSpanTree2 && not (esCiclico g3minSpanTree2))
    putStrLn $ "kruskal g3 (cost3): " ++ show (esConexo g3minSpanTree3 && not (esCiclico g3minSpanTree3))
