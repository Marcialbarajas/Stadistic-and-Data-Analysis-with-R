set.seed(123)
muestra1=rnorm(n=100,mean=5,sd=2)
muestra2=c(read.table(file="muestra2.txt"))
muestra2=muestra2$V1
cat("El valor del primer productorio es:" , prod(muestra1),'\n')
cat("El valor del segundo productorio es:" , prod(muestra2), '\n')
n1=length(muestra1)
n2=length(muestra2)
cat("Apartado 1", '\n')
cat("Ver Gráfico adjunto", '\n')
boxplot(muestra1,muestra2,ylab="Rango de datos", xlab="Muestras", col=c('red','blue'), main="Comparación de la distribución de ambos conjuntos")

cat("Ejercicio 2", '\n')
cat("Apartado A", '\n')
wilcox=wilcox.test(muestra1,muestra2, alternative=c("two.sided", "less", "greater"))
print(wilcox)
cat("Apartado B", '\n')
cat("El nivel de significación con el que se puede rechazar la hipótesis nula de igualdad de medianas es:", wilcox$p.value, '\n')
cat("Apartado C", '\n')
cat("La diferencia no es significativa ya que", wilcox$p.value, "> 0.05", '\n', "Esto significa que se acepta H0, es decir, que aceptamos que las medianas son significativamente iguales", '\n\n')

cat("Ejercicio 3", '\n')
moses.test=function(x,y,k){
  set.seed(123)
  x0=sample(x);
  y0=sample(y)
  m=trunc(length(x0)/k)
  n=trunc(length(y0)/k)
  ci=numeric(m);
  di=numeric(n)
  for (i in 1:m){
    i1=(i-1)*k+1
    i2=i1+k-1
    xmed=sum(x0[i1:i2])/k
    ci[i]=sum((x0[i1:i2]-xmed)^2)
  }
  for (i in 1:n){
    i1=(i-1)*k+1
    i2=i1+k-1
    xmed=sum(y0[i1:i2])/k
    di[i]=sum((y0[i1:i2]-xmed)^2)
  }
    cat("Test de rangos de Moses:",'\n')
  wilcox.test(ci,di)
}
moses=moses.test(muestra1,muestra2,5) #dicen k=5
print(moses)

valor.p2=moses$p.value
cat("El nivel de significación es", valor.p2, "Por lo que rechazamos h0 al ser menor que alpha", '\n')


cat("Ejercicio 4", '\n')
cat("Apartado A", '\n')
ks=ks.test(muestra2,muestra1)
print(ks)

cat("Apartado B", '\n')
valor.p3=ks$p.value

cat("\n","El valor del nivel de significación a partir del cual podemos rechazar la hipótesis
nula de igualdad de distribuciones es:",ks$p.value, '\n')

cat("Apartado C", '\n')
D2=1.36*sqrt((n1+n2)/(n1*n2))
cat("\n","El valor de la diferencia máxima absoluta entre funciones relativas acumuladas es, recurriendo al comando de R:",ks$statistic, '\n')
cat("A mano se puede calcular y da D=", D2 , " un valor bastante aproximado al del test", '\n')