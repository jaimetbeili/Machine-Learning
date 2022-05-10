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


