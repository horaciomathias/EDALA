# EDALA

## Comandos 
cargar archivo .hs
``` ghci GraphsAsFunctions.hs
```

recargar archivo .hs
``` :r
```

ejecutar pruebas
1. Abrir una nueva terminal cmd en la carpeta del proyecto
2. Ejecutar el comando
``` ghc -o pruebas pruebas.hs
```
3. Ejecutar el comando
``` pruebas.exe
```
4. Verificar que todas las pruebas pasen, si la prueba es correcta se imprime true, de lo contrario se imprime false


TODO: Me parece que no está bueno trabajar con grafos infinitos porque no tenemos como ver que la prueba pasa, por lo que se me ocurre que podriamos pasar todas las funciones a Gn en vez de G y hacer las pruebas con grafos finitos
Si decidimos por cambiar a Gn, entonces deberiamos volver a verificar las pruebas que están comentadas porque se quedaban en un loop infinito
- [] verificar que se ejecute la prueba de bfs
- [] verificar que se ejecute la prueba de dfs
- [] verificar que se ejecute la prueba de dijkstra
- [] verificar que se ejecute la prueba de dijkstraClase
- [] verificar que se ejecute la prueba de kruskal