import GAF2

main :: IO ()
main = do
    putStrLn "Pruebas de funciones de grafos"

    -- Pruebas para existeArista
    putStrLn "---COMINEZO EXISTE ARISTA---"
    putStrLn $ "existeArista (1, 2) gn: " ++ show (not (existeArista (1 , 2) gn))
    putStrLn $ "existeArista (1, 1) gn: " ++ show (existeArista (1, 1) gn)
    putStrLn $ "existeArista (3, 3) gAislado: " ++ show (not (existeArista (3, 3) gAislado))
    putStrLn $ "existeArista (4, 5) gAislado: " ++ show (not (existeArista (4, 5) gAislado))
    putStrLn $ "existeArista (1, 1) gSinLazo: " ++ show (not (existeArista (1, 1) gSinLazo))
    putStrLn $ "existeArista (1, 3) gSinLazo: " ++ show (existeArista (1, 3) gSinLazo)
    putStrLn "---FIN EXISTE ARISTA---"

    -- Pruebas para tieneLazoV
    putStrLn "---COMINEZO TIENE LAZO V---"
    putStrLn $ "tieneLazoV 1 gn: " ++ show (tieneLazoV 1 gn)
    putStrLn $ "tieneLazoV 2 gn: " ++ show (tieneLazoV 2 gn)
    putStrLn $ "tieneLazoV 3 gAislado: " ++ show (not (tieneLazoV 3 gAislado))
    putStrLn $ "tieneLazoV 4 gSinLazo: " ++ show (not (tieneLazoV 4 gSinLazo))
    putStrLn "---FIN TIENE LAZO V---"

    -- Pruebas para esAislado
    putStrLn "---COMINEZO ES AISLADO---"
    putStrLn $ "esAislado 1 gn: " ++ show (not (esAislado 1 gn))
    putStrLn $ "esAislado 2 gAislado: " ++ show (esAislado 2 gAislado)
    putStrLn $ "esAislado 3 gSinLazo: " ++ show (not (esAislado 3 gSinLazo))
    putStrLn "---FIN ES AISLADO---"

    -- Pruebas para existeSimetrica
    putStrLn "---COMINEZO EXISTE SIMETRICA---"
    putStrLn $ "existeSimetrica (1, 2) gn: " ++ show (not (existeSimetrica (1, 2) gn))
    putStrLn $ "existeSimetrica (1, 1) gn: " ++ show (existeSimetrica (1, 1) gn)
    putStrLn $ "existeSimetrica (3, 3) gAislado: " ++ show (not (existeSimetrica (3, 3) gAislado))
    putStrLn $ "existeSimetrica (4, 5) gSinLazo: " ++ show (not (existeSimetrica (4, 5) gSinLazo))
    putStrLn $ "existeSimetrica (1, 3) gSinLazo: " ++ show (existeSimetrica (1, 3) gSinLazo)
    putStrLn "---FIN EXISTE SIMETRICA---"

    -- Pruebas para gradoEntrada
    putStrLn "---COMINEZO GRADO ENTRADA---"
    putStrLn $ "gradoEntrada 2 gn: " ++ show (gradoEntrada 1 (gn, 10) == 5)
    putStrLn $ "gradoEntrada 3 gAislado: " ++ show (gradoEntrada 3 (gAislado, 10) == 0)
    putStrLn $ "gradoEntrada 4 gSoloLazo: " ++ show (gradoEntrada 4 (gSoloLazo, 10) == 1)
    putStrLn "---FIN GRADO ENTRADA---"

    -- Pruebas para gradoSalida
    putStrLn "---COMINEZO GRADO SALIDA---"
    putStrLn $ "gradoSalida 1 gn: " ++ show (gradoSalida 1 gn == 5)
    putStrLn $ "gradoSalida 2 gn: " ++ show (gradoSalida 2 gn == 5)
    putStrLn $ "gradoSalida 3 gAislado: " ++ show (gradoSalida 3 gAislado == 0)
    putStrLn $ "gradoSalida 4 gSoloLazo: " ++ show (gradoSalida 4 gSoloLazo == 1)
    putStrLn "---FIN GRADO SALIDA---"

    -- Pruebas para gradoV
    putStrLn "---COMIENZO GRADO V---"
    putStrLn $ "gradoV 1 (gn, 10): " ++ show (gradoV 1 (gn, 10) == 10)
    putStrLn $ "gradoV 2 (gn, 10): " ++ show (gradoV 2 (gn, 10) == 10)
    putStrLn $ "gradoV 3 (gAislado, 10): " ++ show (gradoV 3 (gAislado, 10) == 0)
    putStrLn $ "gradoV 4 (gSinLazo, 10): " ++ show (gradoV 4 (gSinLazo, 10) == 8)
    putStrLn $ "gradoV 4 (gSoloLazo, 10): " ++ show (gradoV 4 (gSoloLazo, 10) == 2)
    putStrLn "---FIN GRADO V---"

        -- Pruebas para aristas
    putStrLn "---COMIENZO ARISTAS---"
    putStrLn $ "aristas (gn, 10): " ++ show (aristas (gn, 10) == [(v1, v2) | v1 <- [0..9], v2 <- gn v1])
    putStrLn $ "aristas (gAislado, 10): " ++ show (null (aristas (gAislado, 10)))
    putStrLn $ "aristas (gSinLazo, 10): " ++ show (aristas (gSinLazo, 10) == [(v1, v2) | v1 <- [0..9], v2 <- gSinLazo v1])
    putStrLn "---FIN ARISTAS---"

    -- Pruebas para grado
    putStrLn "---COMIENZO GRADO---"
    putStrLn $ "grado (gn, 10): " ++ show (grado (gn, 10))
    putStrLn $ "grado (gAislado, 10): " ++ show (grado (gAislado, 10))
    putStrLn $ "grado (gSinLazo, 10): " ++ show (grado (gSinLazo, 10))
    putStrLn "---FIN GRADO---"

    -- Pruebas para listaDeAdyacencia
    putStrLn "---COMIENZO LISTA DE ADYACENCIA---"
    putStrLn $ "listaDeAdyacencia (gn, 10): " ++ show (listaDeAdyacencia (gn, 10) == [(v, gn v) | v <- [0..9], not (null (gn v))])
    putStrLn $ "listaDeAdyacencia (gAislado, 10): " ++ show (null (listaDeAdyacencia (gAislado, 10)))
    putStrLn $ "listaDeAdyacencia (gSinLazo, 10): " ++ show (listaDeAdyacencia (gSinLazo, 10) == [(v, gSinLazo v) | v <- [0..9], not (null (gSinLazo v))])
    putStrLn "---FIN LISTA DE ADYACENCIA---"

    -- Pruebas para matrizDeAdyacencia
    putStrLn "---COMIENZO MATRIZ DE ADYACENCIA---"
    putStrLn $ "matrizDeAdyacencia (gn, 10): " ++ show (matrizDeAdyacencia (gn, 10) == [[v2 `elem` gn v1 | v2 <- [0..9]] | v1 <- [0..9]])
    putStrLn $ "matrizDeAdyacencia (gAislado, 10): " ++ show (matrizDeAdyacencia (gAislado, 10) == replicate 10 (replicate 10 False))
    putStrLn $ "matrizDeAdyacencia (gSinLazo, 10): " ++ show (matrizDeAdyacencia (gSinLazo, 10) == [[v2 `elem` gSinLazo v1 | v2 <- [0..9]] | v1 <- [0..9]])
    putStrLn "---FIN MATRIZ DE ADYACENCIA---"

    -- Pruebas para parVA
    putStrLn "---COMIENZO PAR VA---"
    putStrLn $ "parVA (gn, 10): " ++ show (parVA (gn, 10) == ([0..9], [(v1, v2) | v1 <- [0..9], v2 <- gn v1]))
    putStrLn $ "parVA (gAislado, 10): " ++ show (parVA (gAislado, 10) == ([0..9], []))
    putStrLn $ "parVA (gSinLazo, 10): " ++ show (parVA (gSinLazo, 10) == ([0..9], [(v1, v2) | v1 <- [0..9], v2 <- gSinLazo v1]))
    putStrLn "---FIN PAR VA---"

    -- Pruebas para sacarA
    let g1 = sacarA (1, 2) gn
        g1b = sacarA (1, 1) gn
        g2 = sacarA (3, 4) (agregarA (3, 4) gAislado)
        g3 = sacarA (1, 3) gSinLazo

    putStrLn "---COMIENZO SACAR A---"
    putStrLn $ "sacarA (1, 2) gn 1: " ++ show (g1 1 == filter (/= 2) (gn 1))
    putStrLn $ "sacarA (1, 1) gn 1: " ++ show (g1b 1 == filter (/= 1) (gn 1))
    putStrLn $ "sacarA (3, 4) (agregarA (3, 4) gAislado) 3: " ++ show (g2 3 == filter (/= 4) [4])
    putStrLn $ "sacarA (1, 3) gSinLazo 1: " ++ show (g3 1 == filter (/= 3) (gSinLazo 1))
    putStrLn "---FIN SACAR A---"


    -- Pruebas para esSubgrafo
    let gn1 = (gn, 5)
        gn2 = (gn, 10)
        gAislado10 = (gAislado, 10)
        gSinLazo10 = (gSinLazo, 10)

    putStrLn "---COMIENZO ES SUBGRAFO---"
    putStrLn $ "esSubgrafo (gn, 5) (gn, 10): " ++ show (esSubgrafo gn1 gn2)
    putStrLn $ "esSubgrafo (gn, 10) (gn, 5): " ++ show (not (esSubgrafo gn2 gn1))
    putStrLn $ "esSubgrafo (gAislado, 10) (gn, 10): " ++ show (esSubgrafo gAislado10 gn2)
    putStrLn $ "esSubgrafo (gSinLazo, 10) (gn, 10): " ++ show (esSubgrafo gSinLazo10 gn2)
    putStrLn "---FIN ES SUBGRAFO---"

    let gn1 = (gn, 5)
        gn2 = (complementario (gn, 5), 5)
        gAislado10 = (gAislado, 10)
        gnSinLazo10 = (gSinLazo, 10)

    putStrLn "---COMIENZO SON COMPLEMENTARIOS---"
    putStrLn $ "sonComplementarios (gn, 5) (complementario (gn, 5), 5): " ++ show (sonComplementarios gn1 gn2)
    putStrLn $ "sonComplementarios (gn, 5) (gn, 5): " ++ show (not (sonComplementarios gn1 gn1))
    putStrLn $ "sonComplementarios (gAislado, 10) (gn, 10): " ++ show (not (sonComplementarios gAislado10 gn2))
    putStrLn $ "sonComplementarios (gSinLazo, 10) (gn, 10): " ++ show (not (sonComplementarios gSinLazo10 gn2))
    putStrLn "---FIN SON COMPLEMENTARIOS---"

    -- Pruebas para complementario
    let gn1 = (gn, 5)
        gnComp = complementario gn1
        gAislado10 = (gAislado, 10)
        gAisladoComp = complementario gAislado10
        gSinLazo10 = (gSinLazo, 10)
        gSinLazoComp = complementario gSinLazo10

    putStrLn "---COMIENZO COMPLEMENTARIO---"
    putStrLn $ "complementario (gn, 5): " ++ show (all (\v -> all (\u -> not (existeArista (v, u) gnComp)) (gn v)) (vertices2 gn1))
    putStrLn $ "complementario (gSinLazo, 10): " ++ show (all (\v -> all (\u -> not (existeArista (v, u) gSinLazoComp)) (gSinLazo v)) (vertices2 gSinLazo10))
    putStrLn "---FIN COMPLEMENTARIO---"

    -- Pruebas para esCamino
    let gnTest = gn
        gAisladoTest = gAislado
        gSinLazoTest = gSinLazo

    putStrLn "---COMIENZO ES CAMINO---"
    putStrLn $ "esCamino [1, 2, 3] gn: " ++ show (null (esCamino [1, 2, 3] gnTest))
    putStrLn $ "esCamino [1, 1] gn: " ++ show (esCamino [1, 1] gnTest == [(1, 1)])
    putStrLn $ "esCamino [1, 3] gAislado: " ++ show (null (esCamino [1, 3] gAisladoTest))
    putStrLn $ "esCamino [1, 3, 4] gSinLazo: " ++ show (esCamino [1, 3, 5] gSinLazoTest == [(1, 3), (3, 5)])
    putStrLn "---FIN ES CAMINO---"


    -- Pruebas para clausuraSimetrica
    let gnTest = (gn, 10)
        gnSinLazoTest = (gSinLazo, 10)
        gAisladoTest = (gAislado, 10)
        gSoloLazoTest = (gSoloLazo, 10)

    let gClausura = clausuraSimetrica gnTest
        gSinLazoClausura = clausuraSimetrica gnSinLazoTest
        gAisladoClausura = clausuraSimetrica gAisladoTest
        gSoloLazoClausura = clausuraSimetrica gSoloLazoTest

    putStrLn "---COMIENZO CLAUSURA SIMETRICA---"
    putStrLn $ "clausuraSimetrica (gn, 10) 1: " ++ show (gClausura 1 == (gn 1 ++ [u | u <- [0..9], 1 `elem` gn u, u `notElem` gn 1]))
    putStrLn $ "clausuraSimetrica (gSinLazo, 10) 1: " ++ show (gSinLazoClausura 1 == (gSinLazo 1 ++ [u | u <- [0..9], 1 `elem` gSinLazo u, u `notElem` gSinLazo 1]))
    putStrLn $ "clausuraSimetrica (gAislado, 10) 1: " ++ show (null (gAisladoClausura 1))
    putStrLn $ "clausuraSimetrica (gSoloLazo, 10) 1: " ++ show (gSoloLazoClausura 1 == (gSoloLazo 1 ++ [u | u <- [0..9], 1 `elem` gSoloLazo u, u `notElem` gSoloLazo 1]))
    putStrLn "---FIN CLAUSURA SIMETRICA---"

    -- Pruebas para clausuraTransitiva
    let gnTest = (gn, 10)
        gnSinLazoTest = (gSinLazo, 10)
        gSoloLazoTest = (gSoloLazo, 10)

    let gClausuraTransitiva = clausuraTransitiva gnTest
        gSinLazoClausuraTransitiva = clausuraTransitiva gnSinLazoTest
        gSoloLazoClausuraTransitiva = clausuraTransitiva gSoloLazoTest

    putStrLn "---COMIENZO CLAUSURA TRANSITIVA---"
    putStrLn $ "clausuraTransitiva (gn, 10) 1: " ++ show (gClausuraTransitiva 1 == [u | u <- [0..9], hayCamino gn 1 u])
    putStrLn $ "clausuraTransitiva (gSinLazo, 10) 1: " ++ show (gSinLazoClausuraTransitiva 1 == [u | u <- [0..9], hayCamino gSinLazoTest 1 u])
    putStrLn $ "clausuraTransitiva (gSoloLazo, 10) 1: " ++ show (gSoloLazoClausuraTransitiva 1 == [u | u <- [0..9], hayCamino gSoloLazo 1 u])
    putStrLn "---FIN CLAUSURA TRANSITIVA---"


        -- Pruebas para esCiclico
    let gnTest = (gn, 10)
        gnSinLazoTest = (gSinLazo, 10)
        gAisladoTest = (gAislado, 10)
        gSoloLazoTest = (gSoloLazo, 10)

    putStrLn "---COMIENZO ES CICLICO---"
    putStrLn $ "esCiclico (gn, 10): " ++ show (esCiclico gnTest)
    putStrLn $ "esCiclico (gnSinLazo, 10): " ++ show (esCiclico gnSinLazoTest)
    putStrLn $ "esCiclico (gAislado, 10): " ++ show (not (esCiclico gAisladoTest))
    putStrLn $ "esCiclico (gSoloLazo, 10): " ++ show (esCiclico gSoloLazoTest)
    putStrLn "---FIN ES CICLICO---"


    -- Pruebas para esConexo
    let gnTest = (gn, 10)
        gSinLazoTest = (gSinLazo, 10)
        gAisladoTest = (gAislado, 10)
        gSoloLazoTest = (gSoloLazo, 10)
        gCompletoTest = (const [0 .. 9], 10)

    putStrLn "---COMIENZO ES CONEXO---"
    putStrLn $ "esConexo (gn, 10): " ++ show (not (esConexo gnTest))
    putStrLn $ "esConexo (gSinLazo, 10): " ++ show (not (esConexo gSinLazoTest))
    putStrLn $ "esConexo (gAislado, 10): " ++ show (not (esConexo gAisladoTest))
    putStrLn $ "esConexo (gSoloLazo, 10): " ++ show (not (esConexo gSoloLazoTest))
    putStrLn $ "esConexo (gCompletoTest): " ++ show (esConexo gCompletoTest)
    putStrLn "---FIN ES CONEXO---"


    -- Pruebas para dfs
    let g1 v = case v of
                  0 -> [1, 2]
                  1 -> [2]
                  2 -> [0, 3]
                  3 -> [3]
                  _ -> []
        g2 v = []
        g3 v = [v+1 | v < 9]

    putStrLn "---COMIENZO DFS---"
    putStrLn $ "dfs g1 0: " ++ show (mismosElementos (dfs g1 0) [0, 2, 3, 1])
    putStrLn $ "dfs g1 1: " ++ show (mismosElementos (dfs g1 1) [1, 2, 0, 3])
    putStrLn $ "dfs g1 3: " ++ show (mismosElementos (dfs g1 3) [3])
    putStrLn $ "dfs g2 0: " ++ show (mismosElementos (dfs g2 0) [0])
    putStrLn $ "dfs g3 0: " ++ show (mismosElementos (dfs g3 0) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
    putStrLn "---FIN DFS---"

    -- Pruebas para bfs
    putStrLn "---COMIENZO BFS---"
    putStrLn $ "bfs g1 0: " ++ show (mismosElementos (bfs g1 0) [0, 1, 2, 3])
    putStrLn $ "bfs g1 1: " ++ show (mismosElementos (bfs g1 1) [1, 2, 0, 3])
    putStrLn $ "bfs g1 3: " ++ show (mismosElementos (bfs g1 3) [3])
    putStrLn $ "bfs g2 0: " ++ show (mismosElementos (bfs g2 0) [0])
    putStrLn $ "bfs g3 0: " ++ show (mismosElementos (bfs g3 0) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
    putStrLn "---FIN BFS---"

    -- Función de costo para pruebas
    let c (v1, v2) = if v1 == v2 then 0 else 1

    -- Grafo de prueba
    let gTest v = case v of
                      0 -> [1, 2]
                      1 -> [2, 3]
                      2 -> [3]
                      3 -> []
                      _ -> []

    -- Grafo con pesos
    let gPonderado v = case v of
                          0 -> [1, 2]
                          1 -> [3]
                          2 -> [3]
                          3 -> []
                          _ -> []

    -- Grafo no conectado
    let gNoConectado v = case v of
                             0 -> [1]
                             1 -> [0]
                             2 -> []
                             3 -> []
                             _ -> []

    putStrLn "---COMIENZO DIJKSTRA CLASE---"
    putStrLn $ "dijkstraClase (c, (gTest, 4)) 0 3: " ++ show (dijkstraClase (c, (gTest, 4)) 0 3 == 2)
    putStrLn $ "dijkstraClase (c, (gPonderado, 4)) 0 3: " ++ show (dijkstraClase (c, (gPonderado, 4)) 0 3 == 2)
    putStrLn $ "dijkstraClase (c, (gNoConectado, 4)) 0 3: " ++ show (dijkstraClase (c, (gNoConectado, 4)) 0 3 == 99999)
    putStrLn "---FIN DIJKSTRA CLASE---"


    -- Función de costo para pruebas
    let c (v1, v2) = if v1 == v2 then 0 else v1

    -- Grafo de prueba
    let gTest v = case v of
            0 -> [1, 2]
            1 -> [2, 3]
            2 -> [3]
            3 -> []
            _ -> []

    -- Grafo completo
    let gCompleto v = [u | u <- [0..3], u /= v]

    putStrLn "---COMIENZO KRUSKAL---"
    -- Probar con un grafo simple
    let gKruskal = kruskal c (gTest, 4)
    putStrLn $ "kruskal c (gTest, 4) 0: " ++ show (mismosElementos (gKruskal 0) [1, 2] && mismosElementos (gKruskal 1) [3] && null (gKruskal 2) && null (gKruskal 3))

    -- Probar con un grafo completo
    let gKruskalCompleto = kruskal c (gCompleto, 4)
    putStrLn $ "kruskal c (gCompleto, 4) 0: " ++ show (mismosElementos (gKruskalCompleto 0 ) [1, 2, 3] && null (gKruskalCompleto 1) && null (gKruskalCompleto 2) && null (gKruskalCompleto 3))
    putStrLn "---FIN KRUSKAL---"

    putStrLn "Fin de pruebas de funciones de grafos"