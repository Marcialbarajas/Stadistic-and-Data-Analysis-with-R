#####Apartado 1, función y probabilidad total para 10000 casos#####
simulacion=function(ngente,ncasos){
  coincidencia=numeric(ncasos)
  for (i in 1:ncasos){
    z=sample(365,ngente,replace=T)
    j=unique(z)
    coincidencia[i]=ngente-(length(j))
  }
  return(coincidencia)
}
#Puedo usar los nombres de i,j ya que éstos al estar fuera del bucle no me van a dar problemas.
i=simulacion(15,10000)
j=simulacion(25,10000)
k=simulacion(75,10000)
#Para cada uso de la función hago una tabla y añado los porcentajes para cada número de veces que sale cada caso
freq1=data.frame(table(i))
names(freq1)=c("Nº de veces", "veces")
freq1$Porcentaje=prop.table(freq1$veces)

ProbTotal15=sum(freq1$Porcentaje)-sum(freq1$Porcentaje[1])
print("Tabla de coincidencias con n=15")
print(freq1)
print("Probabilidad total para n=15")
print(ProbTotal15)

freq2=data.frame(table(j))
names(freq2)=c("Nº de veces", "veces")
freq2$Porcentaje=prop.table(freq2$veces)

ProbTotal25=sum(freq2$Porcentaje)-sum(freq2$Porcentaje[1])
print("Tabla de coincidencias con n=25")
print(freq2)
print("Probabilidad total para n=25")
print(ProbTotal25)

freq3=data.frame(table(k))
names(freq3)=c("Nº de veces", "veces")
freq3$Porcentaje=prop.table(freq3$veces)

ProbTotal75=sum(freq3$Porcentaje)-sum(freq3$Porcentaje[1])
print("Tabla de coincidencias con n=75")
print(freq3)
print("Probabilidad total para n=75")
print(ProbTotal75)

#####Apartado 2, hacer barplot y rbind con los valores del vector coincidencia#####
#Hago una cadena para cada caso, asi me aseguro de que me salen las tablas correctamente
i=c(i)
j=c(j)
k=c(k)
#Relleno cada tabla hasta el valor de la tabla más larga, que va a ser k, de ceros para que las longitudes me coincidan
# y asi evitarme problemas de longitudes
tablak=table(factor(k, levels=0:(length(table(k))-1)))
tablai=table(factor(i, levels=0:(length(tablak)-1)))
tablaj=table(factor(j, levels=0:(length(tablak)-1)))
print("Tabla de valores para n=15")
print(tablai)
print("Tabla de valores para n=25")
print(tablaj)
print("Tabla de valores para n=75")
print(tablak)
par(mfrow=c(2,2))
barplot(table(i), xlab="Nº Coincidencias", ylab="Número de veces", main="Tabla de coincidencias para n=15", col="dark blue", ylim=c(0,10000))
barplot(table(j), xlab="Nº Coincidencias", ylab="Número de veces", main="Tabla de coincidencias para n=25", col="green", ylim=c(0,10000))
barplot(table(k), xlab="Nº Coincidencias", ylab="Número de veces", main="Tabla de coincidencias para n=75", col="orange", ylim=c(0,10000))
barplot(rbind(tablai,tablaj,tablak), legend=c("Coincidencias en n=15","coincidencias en n=25","coincidencias en n=75"), axisnames=TRUE, xlab="Nº de coincidencias ", ylab="Nº de veces", col=c("dark blue","green","orange"), main="Unión de los tres casos", beside=TRUE, ylim=c(0,10000))