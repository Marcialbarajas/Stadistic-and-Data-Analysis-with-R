data(airquality)
viento=airquality$Wind
media=mean(viento)
desv=sd(viento)
cat("La media de las medidas del viento es ", media, "y la desviación tipica es ", desv, '\n')
cat("Dividimos los datos del intervalo en 11 para comprobar si siguen una distribucion normal", '\n')
lim=seq(from=0, to=22, by=2)

h=hist(viento, breaks=lim, freq=F, border="gray", xlab="Medias de la velocidad del viento", ylab="Densidad de probabilidad", main="Histograma de probabilidad")
x=seq(min(h$breaks), max(h$breaks), by=1)
y=dnorm(x, mean=media, sd=desv)
lines(x,y,type="l", col="red", lwd=3)

#Con el histograma obtenido, ahora realizamos la tabla de frecuencias observadas

o=h$counts

ninterv=length(lim)-1
Fr=h$density*length(viento)
p=numeric(ninterv)
for (i in 1:ninterv) {
  if (i == ninterv) { # último intervalo
    p[i] = pnorm(lim[i], mean=media, sd=desv, lower.tail=FALSE) # cola derech
  } else {
    if (i == 1) { # primer intervalo
      p[i] = pnorm(lim[i+1], mean=media, sd=desv)
    } else { # no es ni el primer intervalo ni el último
      p[i] = pnorm(lim[i+1], mean=media, sd=desv)-
        pnorm(lim[i], mean=media, sd=desv)
    }
  }
}

e=100*p

freq_observadas=o
freq_esperadas=e
j=cbind(freq_observadas,freq_esperadas)
cat("La tabla esperada es:", '\n')
print(j)
#Ahora, obtenemos los valores de chi y chicrítica para ver si pueden asemejarse a una distribucion normal

chicrit=qchisq(0.05, df=length(Fr)-3, lower.tail=F)
cat("el valor de chi critica es:",chicrit, '\n')
chi=sum((o-e)^2/e)
cat("Mientras que el valor de chi es:", chi, '\n')
cat("ahora veamos si lo cumple mediante chi < chi critica", '\n')
print(chi<chicrit)
cat("Como no se cumple la hipótesis, ya que chi es mucho más grande que chi crítica, los datos no se aproximan a una distribución normal")