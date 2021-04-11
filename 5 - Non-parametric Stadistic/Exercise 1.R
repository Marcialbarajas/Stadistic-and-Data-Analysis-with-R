cat("Ejercicio 1", '\n')#Ejercicio 1
set.seed(123)
datos=read.table("datosXY.txt")
nm=sample(100,20,replace=F)
x=datos$xl[nm]
y=datos$yl[nm]
cat("La suma del producto componente a componente de cada muestra es:", sum(x*y), '\n')

cat("Apartado A", '\n')
cat("Ver gráfica adjunta con las dos rectas", '\n')
plot(x,y, main="Rectas de regresión de la muestra", xlab="x", ylab="y")


pend1=lm(y~x)
abline(lm(y~x), col="red", lwd=2)


cat("Apartado B", '\n')

m1=pend1$coefficients[2]
n1=pend1$coefficients[1]


cat("La pendiente de la recta roja es", m1, "y la ordenada en el origen es", n1, '\n')

cat("Apartado C", '\n')
pend2=lm(x~y)
m2=-pend2$coefficient[1]/pend2$coefficient[2]
n2=1/pend2$coefficients[2]

cat("La pendiente de la recta azul es:", m2, "y su ordenada en el origen es", n2, '\n')
abline(m2,n2,col="blue", lwd=2)

cat("Apartado D", '\n')
#Cálculo del error de los coeficientes de la pendiente
numdatos=length(y)
r=cor(x,y)
sr=sqrt((numdatos-1)*var(y)*(1-r^2)/(numdatos-2))
sdb=sr/(sqrt(numdatos-1)*sd(x))
cat("La desviación típica de la distribución muestral del coeficiente de regresión de la recta es:", sdb, '\n')

cat("Por el comando de R también podemos hallar que es:", summary(lm(y~x))$coefficients[2,2], '\n')
cat("Apartado E", '\n')
cat("Suponemos alpha=0,05", '\n')
summary=summary(pend1)
alpha=0.05
k=summary$coefficients
k1=k[2,4]
cat("es significativamente distinta de cero ya que", k1 ,"< 0.05", '\n')
cat("Apartado F", '\n')
cat("Se acepta con un p value de:", k1,'\n')
cat("Apartado G", '\n')
cat("El coeficiente de correlación lineal es", r, '\n')

cat("Apartado H", '\n')
t=abs(r)*sqrt(numdatos-2)/sqrt(1-r^2)
cat("Nuestro estadístico es", t, '\n')
t.crit=qt(alpha/2, df=numdatos-2, lower.tail=F)
cat("Mientras que el valor crítico es", t.crit, '\n')
cat("Comprobamos si t<t.crit", '\n')
cat(t<t.crit, '\n')
cat("Como es Falso, rechazamos la hipótesis nula de no signifación y calculamos el valor de p:",'\n')
p1=2*pt(t, df=numdatos-2, lower.tail=F)
print(p1)
print("También se puede obtener con la fórmula de correlación")
print(cor.test(x,y)$p.value)

cat("Apartado I", '\n')
rankx=rank(x,ties.method="average")
ranky=rank(y,ties.method="average")
rs=1-6*sum((rankx-ranky)^2)/(numdatos*(numdatos^2-1))
cat("El valor de correlación de Spearman calculado a mano es", rs, '\n')
cat("El coeficiente calculado con el comando de R es:", cor(x,y, method="spearman"), '\n')
cat("Vemos que el valor coincide de las dos maneras", '\n')

cat("Apartado J", '\n')
ts = rs*sqrt((numdatos-2)/(1-rs*rs)); ts
p2= pt(ts, df=numdatos-2, lower.tail=F)*2; p2
cat("El cálculo del valor de p calculado a mano es:", p2, '\n')

sperm2=cor.test(y,x, method="spearman", exact=F)
cat("Mientras que usando el comando en R da", sperm2$p.value, '\n')
cat("Como vemos, es el mismo valor en los dos casos")