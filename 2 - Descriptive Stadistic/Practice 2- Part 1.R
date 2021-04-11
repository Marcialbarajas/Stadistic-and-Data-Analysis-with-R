result1=vector("numeric")
s=function(ngente){
result1=0
for (i in 1:(ngente-1)){
  result1[i]=(1-i/365)
  resultado1=1-prod(result1)
  }
  return(resultado1)
}

s1=s(15)
s2=s(20)
s3=s(23)
s4=s(25)
s5=s(30)
s6=s(40)
s7=s(50)
s8=s(75)

x1=c(15,20,23,25,30,40,50,75)
x2=c(s1,s2,s3,s4,s5,s6,s7,s8)
plot(x1,x2, main="Probabilidad de Coincidencia", ylab="Probabilidad", xlab="Número de personas", pch=4)
print("El valor para n=15")
print(s1)
print("EL valor para n=20")
print(s2)
print("El valor para n=23")
print(s3)
print("El valor para n=25")
print(s4)
print("El valor para n=30")
print(s5)
print("El valor para n=40")
print(s6)
print("El valor para n=50")
print(s7)
print("El valor para n=75")
print(s8)