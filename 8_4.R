#TEMA 1: Distance, Knn
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

#KNN (K-Nearest Neighbor) es el primer algoritmo de machine learning per se que vamos a
#usar. KNN usa principios similares a los de smoothing, basandose en probabilidad
#condicional. La diferencia es que KNN se puede adaptar mejor a informacion en mas de dos
#dimensiones. La idea es que para estimar la probabilidad condicional de que una observa
#sea 7, tomamos los k datos mas cercanos y sacamos un promedio de estos. Probemos como 
#funciona contra un glm.
library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > .5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]
#Para usar un knn utilizamos la formula knn3. Podemos enlistar todos los predictores
#que queremos utilizar en el algoritmo despues del ~ o poner un . que le dice a R que use
#todos.
knn_fit <- knn3(y ~ ., data = mnist_27$train)
#Tambien es posible usar esta funcion creando una matriz con los predictores y luego
#poner los outcomes:
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
#Para esta funcion tenemos que elegir tambien el numero de vecinos a considerar.
#El default es 5.
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)
#Vemos que ya de saque tiene mucho mejor resultado que el glm.
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#La accuracy aumenta pero todavia hay espacio para mejorar. Lo que no queremos que nos
#pase es que el algoritmo quede tan acostumbrado al training set que este "sobreentrenado"
#En nuestro caso, podemos ver que si tratamos de predecir en el training set con el
#algoritmo que tenemos, la accuracy crece mucho mas.
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
#De hecho, si hicieramos el modelo con un solo vecino (k=1), veriamos que la prediccion
#en el training set es casi perfecta, mientras que en el test set seria incluso peor que
#con el glm.
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn_1 <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
#Si vemos este problema con k = 1, probemos una k mucho mas alta. Vemos que no ayuda.
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]
#El problema aqui es el contrario. Una k tan grande no permite flexibilidad y acabamos en
#accuracy parecida al modelo lineal. Se llama a esto oversmoothing.
#Podemos tratar de probar varias k a ver cual es la que mayor accuracy genera.
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
  
})
#El problema aqui es que no se pueden crear algoritmos usando test sets, solo training
#pero mas adelante veremos como escoger una k adecuada sin tener que recurrir al test
#set. En este caso, la k adecuada es 41.
ks[which.max(accuracy$test)]
max(accuracy$test)

#TEMA 2: Cross-validation, and Generative Models
#Para checar que tan bien funciona nuestro algoritmo podemos contra-validar. Para esto
#dividimos nuestro training set k veces, en partes de 1/k que no se sobrepongan. Es
#decir, si tenemos 100 datos y k=5, separamos 1/5 del training set para ver como reacciona
#a los parametros que ya teniamos. Hacemos esto 5 veces, cada vez con un quinto diferente

#Ejercicios
library(tidyverse)
library(caret)
#Nos dan esto, para crear x y y aleatorios. No debemos de poder predecir nada aqui.
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]

#Codigo para hacer una validacion cruzada con metodo glm:
fit <- train(x_subset, y, method = "glm")
fit$results

#Codigo para hacer un t-test que nos de los mejores pvalues en un indice. Con eso podemos
#reconstruir nuestra validacion para que mejore, PERO ES COMO REUSAR TODO EL DATA SET
#PARA HACER EL MODELO. NO SE DEBE DE HACER.
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals <= 0.01)
length(ind)

#Si queremos ver como podrian funcionar diferentes niveles de k:
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#Aplicando el mismo procedimiento a datos anteriores:
data("tissue_gene_expression")
fit <- with(tissue_gene_expression,
            train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results
#Hasta aqui los ejercicios

#Cuando no tenemos acceso a una poblacion, sino a una muestra, podemos usar una tecnica
#llamada bootstraping para simular la poblacion. Empezamos por crear una poblacion de
#ejemplo:
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))
#Aqui esta su promedio.
m <- median(income)
m

#Ahora la convertimos en una muestra:
set.seed(1, sample.kind="Rounding")
N <- 250
X <- sample(income, N)
M<- median(X)
M #y vemos que la media es diferente

#Si corremos una simulacion montecarlo y tomamos los promedios de cada simulacion, tenemos
#una normal:
library(gridExtra)
B <- 10^5
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)
#Con este valor esperado y error estandar:
mean(M)
sd(M)

#El problema de esto es que a veces no conocemos la distribucion, por lo que la simulacion
#montecarlo es insuficiente. La tecnica de bootstraping implica tomar una muestra de la
#muestra (con replacemente, para permitir que se repitan los datos) de forma que hagamos
#una montecarlo dentro de la montecarlo anterior.
B <- 10^5
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})
#Se ve asi
tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

#Estos son los quintiles que necesitariamos para hacer un intervalo de confianza. son
#bastante cercanos:
quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

#Si trataramos de hacer un intervalo de confianza con teorema del limite central nos sale
#esto, que esta equivocado:
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
#Con bootstraping funciona mucho mejor:
mean(M) + 1.96 * sd(M) * c(-1,1)
mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

#Ejercicios
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)
indexes$Resample01

sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

set.seed(1, sample.kind="Rounding")
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q_75)
sd(q_75)

# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
#Hasta aqui ejercicios.

#TEMA 3: MODELOS GENERATIVOS
#Los modelos generativos no solamente predicen usando la distribucion esperada de Y, sino
#tambien las de los predictores X.
#Empecemos con un ejemplo cualquiera: predecir el sexo segun la altura.
library("caret")
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)
#En este caso sabemos que las distribuciones son similares en hombres y mujeres.
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params
#Podemos calcular la prevalencia asi
pi <- train_set %>%
  summarize(pi=mean(sex=="Female")) %>%
  .$pi
pi

x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1-pi))
#Vemos que la prevalencia es de .22, tenemos que controlar la prevalencia.
#Con el enfoque de Bayes esto se puede hacer asi:
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
#Vemos que el modelo privilegia a la especificidad sobre la sensitividad.
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
#En nuestro modelo hace sentido todo esto porque hay mas hombres que mujeres. Si fueramos
#a usar una poblacion dividia 50/50, podemos cambiar la distribucion que usamos en f1 por
#.5
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
#Disminuye un poco la especificidad pero aumenta mucho la sensitividad.
#Con 50% como margen, el algoritmo va a predecir hombre cuando mida mas de 67 aprox. como
#podemos ver en la grafica:
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)
#Vamos a ver un ejemplo mas complicado de esto:
data("mnist_27")
#Tenemos dos predictores, entonces asumimos que la distribucion es normal "bivariable"
#Podemos ver los parametros de esta distribucion cuando es 2 y cuando es 7:
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1,x_2))
params
#Asi se ve de manera grafica:
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = .5)
#(El circulo representa el 95% de los datos)
#Ya que tenemos las distribuciones de los predictores podemos correr un modelo con ellas
#este tipo de modelo se llama QDA
train_qda <- train(y ~ .,
                   method = "qda",
                   data = mnist_27$train)
#La accuracy es bastante buena:
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
#Sin embargo, no es tan bueno como nuestro modelo con kernel. Esto puede ser porque la
#distribucion normal no se acomoda muy bien a los parametros, sobre todo en el caso de
#los 7
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)
#Otro problema del modelo QDA es que conforme aumenta el numero de parametros se tienen
#que calcular demasiados datos y se complica el modelo. Aqui teniamos 2 predictores, pero
#con diez predictores se tienen que calcular todos los promedios, sd y correlaciones. Son
#45 correlaciones. Se vuelve impractico y ademas caemos en overfitting.
#Una opcion para evitar estos probelmas es asumir que todos los parametros tienen la misma
#distribucion y generar un solo valor para los parametros con promedios:
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
#Este tipo de modelo se llama LDA. Se vuelve bastante inflexible por usar menos parametros
#por lo que el accuracy es de hecho algo baja:
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
#Ahora vamos a ver un caso con tres posibles outcomes en lugar de 2. Vamos a tener como
#opciones el 1 el 2 y el 7. Esos datos se consiguen con este codigo largo:
if(!exists("mnist"))mnist <- read_mnist()
set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)
row_column <- expand.grid(row = 1:28, col = 1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)
x <- x > 200
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x))
train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                        x_1 = x[-index_train,1],
                        x_2 = x[-index_train,2])
#En esta grafica se ven los predictores para cada numero.
train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()
#Podemos correr un QDA rapido como ejemplo:
train_qda <- train(y ~ ., method = "qda", data = train_set)
#De entrada vemos que se calculan tres probabilidades. La prob. de que sea 1, la de que
#sea 2 y la de que sea 7. El modelo predice el valor que tenga mayor probabilidad.
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
#La matriz de confusion ahora es de 3x3, el accuracy sigue siendo solo un numero porque
#nos dice que tan seguido predijo de manera correcta. Algo interesante es que hay
#sensitividad y especificidad para cada clase.
confusionMatrix(predict(train_qda, test_set), test_set$y)
#Podemos probar tambien un LDA pero tiene mucha menos accuracy:
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
#O un KNN con mayor accuraccy
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
#La razon por la que no funcionan los modelos QDA ni LDA es porque las distribuciones no
#son normales, particularmente en el caso de los 1, como podemos ver aqui:
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) +
  geom_point(show.legend = FALSE) + stat_ellipse(type="norm")
