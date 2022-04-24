library(tidyverse)
library(caret)
library(dslabs)
data(heights)

#Para este ejemplo vamos a predecir genero utilizando altura:
y <- heights$sex
x <- heights$height

#Normalmente, los datos que conocemos se dividen en dos. La mitad se usa para entrenar al
#algoritmo (training set) y la otra mitad para evaluarlo (test set).
#caret tiene una funcion para hacer esto: createDataPartition. y es la variable a partir,
#times es cuantas veces queremos que lo haga y p es la proporcion que queda en ese set.
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

#Ojo: nos da un indice, no una lista o base de datos. Luego tenemos que usar ese indice
#para seleccionar los que queremos en cada grupo, asi:
test_set <- heights[test_index, ] #Todos los que estan en el indice test.
train_set <- heights[-test_index, ] #Todos menos los que estan en el indice test.

#Ahora vamos a desarrollar el algoritmo usando el training set y luego evaluarlo con el
#test set. Primero vamos a probar el algoritmo mas sencillo posible: atinarle.

y_hat <- sample(c("Male", "Female"),
                length(test_index), replace = TRUE)

#Estamos ignorando completamente la altura y simplemente tratando de atinarle.
#En machine learning por categorias se recomienda usar factores para cada categoria: 
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

#Si tratamos de adivinar, tenemos un overall accuracy de aprox. 52%, cerca de la mitad.
mean(y_hat == test_set$sex)

#Podriamos predecir mejor utilizando la altura porque los hombres son un poco mas altos:
heights %>% group_by(sex) %>%
  summarise(mean(height), sd(height))

#Si tomamos la altura promedio del hombre y le restamos dos sd nos ds 62. Podemos usar
#ese valor como predictor:
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

#Si checamos la overall accuracy, es mucho mayor.
mean(y == y_hat)

#Pero tal vez 62 no sea la mejor opcion donde parar. Podemos checar varias y elegir la
#mejor entre 61 y 70 con la siguiente formula:
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
#Podemos verlo en una grafica donde cutoff son las opciones seleccionadas y accuracy es
#que tan bien funciona cada una.
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
#La accuracy maxima a la que podemos aspirar en este caso es:
max(accuracy) #83% aprox.

#Ese valor se encuentra en 64, como podemos ver aqui.
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Es importante recordar que llegamos a este nivel de accuracy usando el training set, lo
#que puede llevarnos a un resultado demasiado optimista. En realidad, debemos de evaluar
#el algoritmo con el test set. Vamos que la accuracy es de 81%. Menos que el 83% que
#teniamos antes, pero mas realista y aun mejor que el 50% de atinarle.
y_hat <- ifelse(test_set$height > 64, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)