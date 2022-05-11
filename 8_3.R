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













