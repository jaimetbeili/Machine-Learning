#TEMA 1: Distancia
library(dslabs)
library(tidyverse)
set.seed(0, sample.kind = "Rounding")

#Asi como en el plano cartesiano existe la distancia entre dos puntos, en machine learning
#podemos hablar de la distancia teorica entre dos outcomes.
#La distancia se calcula como la raiz de la suma del cuadrado de la resta de dos puntos.
#Es decir, la distancia entre x y y seria sqrt(sum((x - y)^2)). El tema es que 1 y 2 no
#son los puntos de los que vamos a calcular la distancia, sino sus predictores. En otras
#palabras, x y y son vectores con muchos datos. Restamos estos dos vectores y sumamos los
#resultados de esa resta elevados al cuadrado y finalmente aplicamos raiz cuadrada. Es
#importante destacar que esta es una distancia conceptual, abstracta. Es como pensar en la
#distancia entre un conejo y un aguila. Podemos senalar factores (predictores) como las 
#alas, las orejas, el pico, etc, con los que podemos apreciar esa distancia, pero no es
#que las alas y las orejas sean la disntancia fisica entre ambas especies.
#Empecemos por cargar la base de datos de los digitos y nos quedamos con 500 aleatorios 
#que sabemos son 2 y 7.
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

#Los primeros tres digitos que tenemos son 7, 7 y 2. 
y[1:3]
#Estos son los predictores para cada uno de esos tres numeros.
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
#Y estas son las distancias entre ellos. Vemos que la menor distancia es la primera. Es
#decir, si los numeros son iguales, la distancia entre sus predictores es menor.
sqrt(sum((x_1 - x_2)^2)) #Distancia entre el primer 7 y el segundo 7.
sqrt(sum((x_1 - x_3)^2)) #Distancia entre el primer 7 y el 2.
sqrt(sum((x_2 - x_3)^2)) #Distancia entre el segundo 7 y el 2.

#En algebra matricial, esto mismo se puede calcular asi:
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#La funcion dist en R calcula la distancia entre cada fila de una matriz. Nos da un objeto
#de clase dist.
d <- dist(x)
class(d)
#Se pueden hacer muchas cosas con un objeto tipo dist, pero para ver las distancias, lo
#tenemos que convertir en una matriz. Aqui podemos ver las distancias entre nuestros tres
#primeros digitos. Vemos como coinciden con los valores calculados. Tambien destaca que
#los valores x,y y y,x son iguales, pues ambos miden la distancia entre x y y.
as.matrix(d)[1:3,1:3]
as.matrix(d)[1:5,1:5]
#Podemos pasar esto a una imagen. Aqui se ve toda desordenada, es la distancia entre cada
#par de numeros acomodados como estaban cuando sacamos el sample en la linea 20.
image(as.matrix(d))
#Aqui ya los tenemos acomodados. Vemos que la distancia entre los 7 es menor (cuadrante
#arriba derecha) que la distancia entre los 2 (abajo izquierda). Esto quiere decir que hay
#mayor uniformidad en como se dibujan los 7 que los 2. Por otro lado, la distancia entre
#los 7 y los 2 es alta (arriba izquierda y abajo derecha), pues es la mas roja.
image(as.matrix(d)[order(y), order(y)])

#Hasta ahora calculamos la distancia entre los predictores de dos digitos, pero tambien
#podemos calcular la distancia entre cada pixel. Es decir, entre cada predictor. Si la
#funcion dist calcula las distancias entre filas, dist(t()) calcula la distancia entre
#columnas, pues t es la transpuesta. Tenemos una matriz con la distancia entre el valor de
#cada pixel y el de los demas pixeles.
d <- dist(t(x))
dim(as.matrix(d))
#Podemos tomar cualquier pixel y ver cuales tienen poca distancia matematica respecto a 
#este. Probamos con el pixel 492.
d_492 <- as.matrix(d)[492,]
#Los pixeles mas cercanos al 492 tienen un valor similar. Esto tiene mucho sentido, pues
#la poca distancia matematica en este caso implica que tienen la misma cantidad de tinta.
#Un pixel probablemente tendra la misma cantidad de tinta que los mas cercanos a este en
#cada dibujo.
image(1:28, 1:28, matrix(d_492, 28, 28))



















