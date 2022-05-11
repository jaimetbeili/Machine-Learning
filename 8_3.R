library(tidyverse)
library(HistData)

#Supongamos que tenemos que hacer una algoritmo para predecir la altura del hijo usando
#la altura del padre. Primero cargamos los datos.
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Ahora generamos grupos de prueba y de entrenamiento.
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#Si tratamos de adivinar la altura del hijo ignorando la del padre, nos vamos con el
#promedio de altura de hijos.
avg <- mean(train_set$son)
#Nuestra perdida de R2 es de 6.9 aprox
mean((avg - test_set$son)^2)

#Podriamos tener menor perdida. Recordemos que si dos variables siguen una distribucion
#normal, la regresion nos dice cual es el valor estimado de Y sujeto a la condicion de X.
fit <- lm(son ~ father, data = train_set)
fit$coef

#La altura del hijo es 37.5 + 0.48*(altura del padre)
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
#Y si predecimos con esto, la perdida es de 4.9 aprox.
mean((y_hat - test_set$son)^2)

#Antes de continuar, expliquemos la funcion predict.
#Esta funcion toma una regresion ya hecha pero con una nueva df de predictores y devuelve
#una prediccion a partir de esta
#En lugar de poner la formula que usamos para y_hat podriamos hacer esto:
y_hat <- predict(fit, test_set)
#Nos sigue dando la misma perdida.
mean((y_hat - test_set$son)^2)
#Mas informacion en como se usa esto se puede leer aqui:
?predict.lm
?predict.glm
?predict.Arima
#Y hay muchos mas

#ESOTS EJERCICIOS FUERON PARTICULARMENTE COMPLEJOS
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
mean(rmse)
sd(rmse)

aver <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), desv = sd(rmse))
}

set.seed(1, sample.kind = "Rounding")
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, aver)

set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$y)^2))
#HASTA AQUI LOS EJERCICIOS

#Las regresiones tambien se pueden usar para predecir en categoricos. Regresemos al
#ejemplo de tratar de predecir genero usando altura.
library(dslabs)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding")

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#La pregunta es cual es la probabilidad condicional de ser mujer si miedes 66 inches.
#Dado que: mujer Y=1 y hombre Y=0, buscamos pr(Y=1|x=66)
#En nuestro train set el 24% de quienes miden 66 in son mujeres:
train_set %>% 
  filter(round(height)==71) %>%
  summarize(y_hat = mean(sex=="Female"))

#Podemos hacer esto para cada inch. Redondeamos, agrupamos por altura, filtramos los que
#tengan muy pocos datos y graficamos la proporcion que son mujeres:
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()
#La grafica se ve casi lineal, asi que usemos una regresion:
#Cambimos el dato de mujer por numeros y estimamos
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
  lm(y ~ height, data = .)
#Lo que esto nos dice es la probabilidad de que sea mujer. Podemos usar este modelo para
#generar predicciones. Que tan probable es que cada dato del test set sea mujer:
p_hat <- predict(lm_fit, test_set)
#Ya que tenemos estas probabilidades, tenemos que determinar cuando queremos que el
#algoritmo prediga mujer. Aqui decidimos que cada vez que la probabilidad de que sea
#mujer este arriba del 50% esa sera la prediccion del algoritmo:
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
#La overall accuracy, en este caso, es de 78.5%
confusionMatrix(y_hat, test_set$sex)

#Sucede que usando el modelo anterior, la linea resultante va de datos negativos a datos
#mayores a 1. Esto no tiene sentido. Si 1 es mujer y 0 es hombre, la probabilidad deberia
#de estar entre estos dos valores
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])
#Se puede evitar esto usando un tipo de regresion diferente: regresion logistica
#Lo que esto hace es usar una transformacion logaritmica log(p/(1-p)) para estimar.
#Para esto vamos a usar la funcion glm() general linear models. Esta funcion es mas
#general, por lo que tenemos que decirle el tipo de modelo que queremos en "family":
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data = ., family = "binomial")
#Con un glm tambien podemos predecir, pero tenemos que incluir type = "response":
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
#De lo contrario, tendremos datos que siguen fuera del rango entre 0 y 1.
#Podemos ver que ya se adecua mucho mejor el modelo.
tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

#Volvemos a la regla de perdecir femenino con p mayor a 50% y tenemos acuraccy un poco
#superior a la que teniamos anteriormente.
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)

#CASO DE ESTUDIO: 2 o 7
#En un caso mas complejo tenemos mas de un predictor y mas de un outcome. En este caso
#vamos a tener dos de cada uno: vamos predecir si un digito escrito a mano es un 2 o un 7
#los predictores que vamos a usar son la proporcion de pixeles oscuros en el cuadrante
#de arriba-izquierda y la proporcion de pixeles oscuros en el cuadrante abajo-derecha.
#Imaginar que tomamos la foto de un digito y lo dividimos en cuatro.
#Tenemos un dataset de 60,000 datos. Seleccionaremos 10,000 para hacerlo mas manejable.
#500 para el training y 500 para el test. Aqui ya vienen solo 10,000:
data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
#De entrada podemos ver un patron. Por ejemplo, a mayor x_1 (que representa el cuadrante
#arriba a la izquierda) es mas probable que sea 7. Cuando x_2 esta en valores no muy
#extremos es mas probable un 2.
#Para ver como se relacionan estos insights con la informacion que tenemos, podemos ver
#las fotos de los digitos con mayor x_1 y menor x_1.
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1),
                             which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)
#El de la izquierda tiene mucho negro en el cuadrante arriba-izquierda y es un 7. El de
#la derecha no tiene casi nada de negro en ese cuadrante y es un 2. Se confirma lo visto
#en la grafica de colores.
#Ahora veamos los digitos con los valores mas altos y mas bajos de x_2:
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2),
                             which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)
#Se comprueba que para valores muy extremos de x_2 vemos 7.
#Ahora si, podemos empezar a programar un algoritmo. Empecemos con un modelo logistico.
#Estamos calculando la probabilidad condicional de que y sea un 7 dados los valores de
#x_1 y x_2.
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)

#En este caso conocemos la verdadera probabilidad condicional de que un numero se 7 o 2
#dados los valores de x_1 yx_2. En la practica no es asi, pero podemos ver que tan 
#certero es nuestro algoritmo viendo las probabilidades condicionales verdaderas.
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_tile()
#Se ve mejor asi:
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 
#Vemos que las probabilidades de tener un 7 son mayores al 50% en lo azul y menores en
#lo rojo. Aqui nos damos cuenta de las limitantes que tiene este tipo de modelo. Asi se
#ve la grafica de probabilidad condicional usando nuestro modelo:
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 
#Esta regresion solo nos va a aportar una linea recta que divide entre los dos grupos, no
#una curva como la de la grafica pasada. Para ver donde estan nuestros errores podemos
#volver a poner los puntos.
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)
#Algunos 7 quedan del lado del 2 y viceversa. Vamos a ver a continuacion formas de 
#atender un problema asi con aproaches mas complejos, empezando con ejemplos sencillos.

#TEMA 2:
#Vamos a explicar una tecnica llamada smoothing. Basicamente es quitar el noise.
#Para hacerlo utilizaremos datos de la eleccion entre obama y mccain de 2008. Para este
#caso no nos importa predecir, nos importa entender la forma en que se mueve la info.
data("polls_2008")
qplot(day, margin, data = polls_2008)
#La encuesta de cada dia (x) muestra una preferencia (f(x)) a la que tenemos que anadir
#un error (epsilon) por la encuestadora. Eso nos da una preferencia reportada (y) tal
#que: y = f(x) + eps
#Dado que hay dias con mucha variabilidad, una regresion lineal no sera suficiente para
#este problema. Nos puede llegar a pasar lo mismo que con el de 2 o 7. Para evitarlo,
#usaremos un metodo mas flexible.
#Bin Smoothing & Kernels
#Bin smoothing es basicamente estratificar los datos en grupos en los que f(x) pueda ser
#relativamente constante. Podemos asumir esto si f(x) se mueve lentamente. Por ejemplo, 
#podemos asumir que las preferencias se mantienen relativamente parecidas dentro de una
#misma semana.
#El span o bandwidth es la ventana de tiempo que estamos tomando. f(x) va a ser el 
#promedio en esa ventana de tiempo para la ventana que tiene cada x. En este caso, cuando
#tomamos un dia x, ese dia queda en el centro y la semana se mide usando los 3.5 dias
#previos y los 3.5 dias siguientes. Como podemos ver en esta grafica eso implica que cada
#vez que tomamos un nuevo dia x para nuestro centro cambian dos puntos, nos movemos hacia
#los dos lados. Esto hace que la curva quede "spiky":
span <- 7
fit <- with(polls_2008,ksmooth(day, margin, x.points = day,
                               kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")
#Para mejorar esto podemos cambiar el argumento kernel en la funcion ksmooth por el 
#normal. Lo que esto hace es ponderar el promedio de cada bin, dando mayor peso a las x
#mas cercanas al centro. Asi, si nuestra x del centro es el dia -43, ese sera el que mas
#peso tenga en el promedio. Los extremos tendran mucho menor peso y la grafica no se vera
#tan marcada en los cambios de ventana.
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day,
                                kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")
#Hay muchas formas de usar kernels en R. Aqui se mostro k-smooth. Sin embargo, no siempre
#nos da los resultados adecuados. En algunos lugares la funcion sigue muy movida. Se 
#suelen preferir otros metodos, como los que se presentaran a continuacion.
#El teorema de Taylor nos indica que toda curva esta construida por rectas locales. Tal
#vez la grafica completa sea una curva, pero de un punto a otro se ve una linea. Una
#regresion ponderada local (loess) corre una pequena regresion lineal en periodos locales
#previamente determinados (span) y nos da un valor predecido para cada periodo. Esto nos
#da una curva mas smooth y con mas valores que los kernels que usamos antes.
#Aqui el span es de 3 semanas:
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")
#Ojo: el span que le damos a loess es una proporcion, no un entero. Lo que loess hace es
#tomar esa proporcion y multiplicarla por el numero total de datos. Eso significa que si
#el span es de .2 loess tomara el 20% de los datos mas cercanos a cada punto. 
#Loess toma un promedio pondeado utilizando una formula diferente a la de los kernels.
#Loess tambien puede eliminar outliers con el argumento family = symetric
fit <- loess(margin ~ day, degree = 1, span = span,
             data = polls_2008, family = "symmetric")
polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

#Otra cosa a considerar: De acuerdo con el teorema de Taylor, si vemos una funcion de
#cerca, pero no tan cerca para que sea una linea, se vuelve una parabola. La funcion de
#loess tiene como default hacer las pequenas regresiones como parabolas, no como lineas.
#Para cambiar eso se usa el argumento degree, que senala el grado de polinomio que se 
#quiere utilizar. El default es 2 (polinomio cuadrado). Usar el 1 reduce el ruido, pero
#ambos tienen su utilidad.
fit <- loess(margin ~ day, degree = 2, span = span, data = polls_2008)
polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

#ggplot puede hacer todo esto de manera automatica:
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))
#Aunque hay que tener cuidado de no usar los defaults porque no quedan tan bien:
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth()


#TEMA 3:
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

#Cuando tenemos muchos predictores numericos y muchos outcomes, es conveniente guardar
#los predictores en una matriz separada. En los datos de mnist, esto ya esta hecho:
class(mnist$train$images)
#Esta matriz tiene los 60,000 datos. Para hacerlo mas facil tomaremos los primeros 1,000:
x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]
#x es la matriz de los predictores y y son los resultados:
class(x)
class(y)
#Las matrices son utilie para hacer operaciones complicadas y necesarias para machine
#learning. Vamos a tratar de realizarlas para lograr cinco objetivos relacionados con
#predecir digitos:
#1. Ver cuanto varia la densidad de pixeles en cada digito.
#2. Eliminar pixeles que no cambian mucho entre digitos y que no nos ayudan a predecir.
#3. Eliminar las manchitas que quedan al rededor de la escritura.
#4. Convertir los datos a binarios donde exista escritura y donde no (1 y 0).
#5. Poner todo a escala.
#Pero primero es necesario introducit conceptos de algebra linel en R.
#En algebra matricial hay tres tipos de datos: scalars (numeros), vectores y matrices.
#El primer vector de la matriz x tiene 1000 scalars:
length(x[,1])
#Una matriz se construye por vectores del mismo tamano. Por ejemplo: x_1 y x_2 son
#vectores y se pueden juntar en una matriz:
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
#La funcion dim nos da la dimension de la matriz (filas luego columnas)
dim(x)
#mil filas 784 columnas. Los vectores no tienen dimension, pero se les puede convertir en
#matrices usando as.matrix para que nos de su dimension.
dim(x_1)
dim(as.matrix(x_1))

#Suele ser util convertir vectores en matrices. Por ejemplo, si tenemos un vector que nos
#dice la intensidad de color en un pixel. Como los pixeles se acomodan en un plano con
#forma de matriz, seria util convertir este vector a una matriz. Para convertir vectores
#a matrices se puede usar la funcion matrix:
my_vector <- 1:15
my_vector
mat <- matrix(my_vector, 5, 3)
mat
#Ojo: el vector tiene los numeros del 1 al 15. La funcion matrix me pide el vector a 
#convertir, luego el numero de filas y luego el de columnas y va llenando columna por
#columna utilizando los numeros del vector en orden a menos que le indiquemos hacerlo
#primero por filas y luego por culumnas:
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
#La matriz queda transpuesta y no es igual a la anterior:
identical(mat, mat_t)
#En R podemos transponer directamente con la funcion t:
identical(t(mat), mat_t)
#R recicla los valores si sobran columnas o filas, por lo que hay que tener cuidado si la
#longitud del vector no corresponde al numero de filas y columnas:
matrix(my_vector, 5, 5)
#En la practica podemos usarlo de esta forma:
#Hacemos una matriz llamada grid usando el vector 3 en 28 columnas y 28 filas. Recordemos
#que una imagen en este caso tiene 784 = 28*28 pixeles
grid <- matrix(x[3,], 28, 28)
#Convertimos esta matriz a una imagen usando esta funcion:
image(1:28, 1:28, grid)
#Toma el numero de filas y columnas (1 a 28) y lo rellena con los datos del tercer
#argumento (grid). La imagen sale al reves porque se empieza a construir de abajo hacia
#arriba, pero la podemos voltear:
image(1:28, 1:28, grid[,28:1])
#Parece un 4. Se comprueba asi y funciona con todos los numeros de la base de datos:
y[3]
#Prueba aleatoria
rand <- sample(1:1000, 1, replace = FALSE)
image(1:28, 1:28, (matrix(x[rand,], 28, 28))[,28:1])
y[rand]

#rowSums toma la suma de cada fila y rowMeans el promedio de cada fila.
sums <- rowSums(x)
avg <- rowMeans(x)
#Si agrupamos y por digitos y graficamos segun el promedio de cada fila, vemos que el 
#numero uno es el que menos tinta usa en promedio para cada numero, lo cual tiene mucho
#sentido:
data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")
#Objetivo 1 alcanzado.
#Esto tambien se puede hacer por columnas con colSums y colMeans
#El paquete matrixStats tiene funciones adicionales para operar en filas y columnas. Por
#ejemplo rowSds y colSds
#Existe tambien la posibilidad de hacerlo con la funcion apply. Para ello, le damos tres
#argumentos: la matriz, la dimension (1=filas,2=columnas) y la funcion a aplicar
avgs <- apply(x, 1, mean) #Esto seria rowMeans
sds <- apply(x, 2, sd) #Esto seria colSds
#Esta flexibilidad de aplicar cualquier funcion viene con el problema de que no son tan
#rapidas como las especializadas.

library(matrixStats)
#Para nuestro segundo objetivo queremos ver los pixeles que casi no cambian. Cada columna
#representa un pixel. Algunos pixeles tienen muy poca variabilidad
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
#En las orillas la variacion es mucho menor, pues solemos escribir en el centro:
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])
#Podemos quitar los pixeles que no nos generan ayuda predictiva.
#Recordemos que con este codigo extraen columnas:
x[ ,c(351,352)]
#Y con este filas.
x[c(2,3),]
#Aqui lo que estamos haciendo es extraer solo las columnas con ds > 60 y guardarlas en
#una nueva matriz new_x
new_x <- x[ ,colSds(x) > 60]
#new_x sigue teniendo 1000 digitos, pero ahora solo 314 pixeles en lugar de 784.
dim(new_x)
#Objetivo 2 alcanzado.
#Una nota importante: new_x es una matriz porque incluimos mas de una columna. Si solo se
#incluye una fila o una columna a la hora de hacer una submatriz, esta se convierte en un
#vector y ya no se puede utilizar como matriz, a menos que se incluya el argumento DROP:
class(x[,1])
dim(x[1,])
class(x[,1, drop=FALSE])
dim(x[,1, drop=FALSE])

#Asi como podemos convertir vectores en matrices, lo contrario se hace asi:
mat <- matrix(1:15, 5, 3)
as.vector(mat)
#Asi se veria un histograma de todos nuestros vectores.
qplot(as.vector(x), bins = 30, color = I("black"))
#Se ve clara la diferencia entre lugares con tinta y sin tinta. Si creemos que los
#valores a 25 son manchitas podemos retirarlos y convertirlos en cero de esta forma:
new_x <- x
new_x[new_x < 50] <- 0
#Objetivo 3 alcanzado.
#Lo que sucede es basicamente esto:
mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat
#Funciona tambien con operaciones un poco mas complejas.
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat
#Ahora queremos poner los datos en 1 y 0 segun si tienen tinta o no. La intensidad de
#color va de 0 a 255. Podemos asumir que si tiene al menos la mitad de esa intensidad, 
#entonces si hay tinta.
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
#Objetivo 4 alcanzado.
#Para el objetivo 5 queremos estandarizar todo para que tenga el mismo promedio y ds.
#En R si le restamos un vector a una matriz lo que sucede es que el primer numero del
#vector se resta a la primera fila, el segundo numero del vector a la segunda fila, etc:
mat <- matrix(6:20, 5, 3)
mat
vec <- c(1:5)
mat - vec #6, 11 y 16 menos 1; 7, 12 y 17 menos 2...
#Lo mismo sucede con cualquier otra operacion. Podemos poner a escala las filas usando
#esta operacion:
(x - rowMeans(x)) / rowSds(x)
#Para hacer lo mismo con  las columnas habria que transponer la matriz:
t(t(x) - colMeans(x))
#Tambien se pude usar para esto la funcion sweep. Funciona de manera parecida a apply.
#Sustrae el vector colMeans(x) de las columnas (por eso el 2).
x_mean_0 <- sweep(x, 2, colMeans(x))
#La operacion default que realiza sweep es restar, pero podemos pedir otra. Por ejemplo,
#para dividir por la ds y ya tener x estandarizada, se hace asi:
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")
#Objetivo 5 alcanzado.
#Comandos adicionales de utilidad:
#La multiplicacion de matrices se hace asi:
x %*% x
#Una matriz por su transpuesta se hace asi:
crossprod(x)






