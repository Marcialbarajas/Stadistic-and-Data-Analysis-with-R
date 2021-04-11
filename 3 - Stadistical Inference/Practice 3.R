set.seed(123)
#Generamos un vector con 100 muestras que cumpla la media de 5 y la distribución tipica de 2
muestra=rnorm(100,mean=5,sd=2)
#A continuacion verificamos que lo cumpla:
med=mean(muestra)
desv=sd(muestra)
hist(muestra, breaks=10, freq=F, main="Distribución muestral de la media", xlab=expression(bar(X)),col="light blue", ylab="f(x)",ylim=c(0,0.3))
curve(dnorm(x,mean=5,sd=2), from=-5,to=25,add=T,col="red")
text(9, 0.25, "N(5,2)", cex=1.5, col="red")
##APARTADO 3, n>30 y sigma desconocida
#De esta parte sabemos que la desviación de la muestra, al ser un n suficientemente grande
#tomamos el valor de la desviación de la muestra como la desviación.
#valor de la alpha_05
alpha_05=qnorm(0.05,lower.tail=F)
center=mean(muestra)
y1=center + alpha_05*sd(muestra)/sqrt(length(muestra))
y2=center - alpha_05*sd(muestra)/sqrt(length(muestra))
#como cabe esperar, al no ser una muestra adaptada a la normal, si hacemos el comando r.test en este apartado
#los valores del intervalo de confianza no van a ser los mismos que se ajusten a una distribución normal
##Apartado 4, consideramos una aproximación normal.
n=length(muestra)
center=med
t_0.5=qt(0.05, df=n-1,lower.tail=F)
width05=t_0.5*sd(muestra)/sqrt(n)
##Apartado 5. Intervalo de confianza del 95% para la desviación típica poblacional
nc=95
lim=(1-nc/100)/2
chi1=qchisq(lim, df=length(muestra)-1, lower.tail=F)
chi2=qchisq(lim, df=length(muestra)-1)
l1=sqrt((length(muestra)-1)*var(muestra)/chi1)
l2=sqrt((length(muestra)-1)*var(muestra)/chi2)
#Como vemos, el intervalo no es simétrico en el centro.
######Texto por pantalla#####
cat("Apartado 1", '\n')
cat('\n')
cat("la media de la muestra es",med, '\n')
cat('\n')
cat("la desviación típica es", desv,'\n')
cat('\n')
cat("Apartado 2",'\n')
cat('\n')
cat("Es la gráfica representada",'\n')
cat('\n')
cat("Apartado 3", '\n')
cat('\n')
cat("El intervalo de confianza del 90% de la media poblacional sin aproximación a una normal es: ",'\n')
cat('\n')
cat("[",y2,",",y1,"]", '\n')
cat('\n')
cat("Mientras que el intervalo de confianza del 90% mediante el comando t.test es:",'\n')
cat('\n')
print(t.test(muestra, conf.level=0.9))
cat("Apartado 4",'\n')
cat('\n')
cat("El intervalo de confianza del 90% de la media poblacional con aproximación a la normal es", '\n')
cat('\n')
cat("[",center-width05,",",center+width05,"]", '\n')
cat("Comprobando con el comando t.test, vemos que el valor coincide",'\n')
print(t.test(muestra, conf.level=0.90))
cat('\n')
cat("Apartado 5", '\n')
cat ("El intervalo de confianza del 95% para la desviación típica poblacional queda como", '\n')
cat('\n')
cat("[",l1,",",l2,"]","\n")