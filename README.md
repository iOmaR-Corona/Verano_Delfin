# Verano_Delfin
Metodología para recrear los resultados del trabajo generado en el verano de investigación 2020

El primer documento que se tiene que compilar es el llamado pruebascript1mes.R, es el encargado de generar los histogramas de cada estado, calculando las correlaciones que existen con el delito número [1], de momento solo funciona estado por estado, esto para que no demore demasiado el tiempo de compilación, sin embargo, se puede hacer para todos los municipios agregando un loop for con el vector c(1:32), aplicado a la linea 15 # datostotales<-datostotales[clave_ent==32] # cambiar el 32 por la variable principal del loop for.

Lo siguiente a realizar es unir todos los datos recaudados por los histogramas, es decir, reunir los 32 estados en un solo data.frame, esto se realizó en el archivo estados_cluster1mes.R, dependiendo de como se guarden los archivos del apartado anterior, se deberan cambiar los nombres en este nuevo. Se devolverá un data.frame que incluya los posibles predictores distribuidos en 5 clusters.

### Para encontrar los municipios más violentos utilizar el archivo municipiosviolentos.R ###

Con los datos obtenidos hasta el momento es posible entrenar una red neuronal con datos propios de cada municipio, para ello es suficiente con compilar el archivo neuralnettrainers.R hasta la línea [132] e imprimir la variable resultadoscluster4 para observar 10 resultados de cada municipio, si se quiere visualizar mejor a que municipio corresponde cada resultado es suficiente con descomentar la linea [28]   #  cat(k,"\n") 

### Para calcular los vecinos y agregarlos a un data frame ### 

Compilar el archivo vecindades.R, el cual arrojará un archivo llamado adyacencias.csv que contiene todos los municipios con sus adyacencias. (Las adyacencias se encuentran en un formato distinto de la clave de municipio que se ha venido manejando)

Para transformar ese etiquetado que se ha generado en las adyacencias es necesario compilar el archivo adyacenciasynombres.R (solo la parte de #calculo de adyacencias), dicho script nos dará un archivo que contiene los 50 municipios en la primera columna y sus vecinos adyacentes en las columnas posteriores.

Para saber cuales de los municipios son útiles se debe implementar el código en relacionvecinos.R, el cual aún no se encuentra optimizado pues se deben colocar manualmente el numero de fila en la que se encuentra el municipio y las columnas donde se encuentran sus vecinos.** De esta manera se obtuvo el documento vecyadj1.csv que contiene los vecinos utilizados en la red.

### Generar data.frames de cada municipio ###
Se debe generar en primera instancia un data frame que contenga los delitos de cada municipio junto con los delitos de sus vecinos. Para ello se utiliza generartablann.R al cual de momento se le debe simplemente cambiar el valor de i desde 1 a 50.

### Para compilar la red con vecinos ###

Para compilar la nueva red, es necesario usar el archivo neuralnetvecinos.R Si se compila todo el script lo que se obtendrá es una serie de imágenes que comparan los resultados de utilizar datos solo del municipio y utilizando datos de vecinos. Por lo que en este script se pueden realizar los dos resultados, si se quieren solo los resultados del municipio, compilar el primer loop for y descomentar la línea para escribir el archivo csv, por el contrario si se quieren los resultados que contienen los vecinos es compilar el segundo loop y también descomentar la línea que escribe los archivos csv. líneas [93 y 151]

*Con los resultados de los vecinos, se seleccionó manualmente el mejor de cada 10 predicciones y se unieron en un data.frame llamado prediccionconvecinosjunio.csv*

Con el cual se generaron por mes todos los resultados, de esta manera poder crear un mapa para cada mes, con el archivo mapaprediccion.R (en el cual se puede usar la herramienta de buscar y reemplazar para cambiar el mes en el script, los meses que se deben incluir es marzo, abril,mayo,junio)

Por último, para generar una tabla de errores se utiliza el archivo erroresmedioabs.R


