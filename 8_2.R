#NOTA: 8_1 FUE TOTALMENTE TEORICO, POR LO QUE NO HUBO R SCRIPT.

library(tidyverse)
library(caret)
library(dslabs)
data(heights)

#TEMA 1:
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

#Si tomamos la altura promedio del hombre y le restamos dos sd nos da 62. Podemos usar
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

#TEMA 2:
#Llegamos a 64, pero el promedio de altura de las mujeres es 65. No esta mal que cuando
#sean mas altos de 64 prediga hombres si el promedio de altura de mujeres es mayor a eso?
#De hecho, en esta tabla podemos ver que si tenemos un problema. El modelo es mucho mas
#adecuado para adivinar si se trata de un hombre (93.3%) que una mujer (42%). En total,
#nuestro modelo parece ser muy adecuado, pero predice muy mal para las mujeres. Esto se
#debe a que hay mas hombres que mujeres en nuestro set.
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

#77.3% de los participantes son hombres. Los errores en mujeres se compensan porque hay
#muchos aciertos en hombres.
prev <- mean(y == "Male")
prev

#Un training set sesgado puede debilitar el algoritmo. Por eso, se usan mas datos, no 
#solo el overall accuracy. Estos datos se pueden conseguir con la matriz de confusion.
#A general improvement to using overall accuracy is to study sensitivity and specificity
#separately.
#Sensitivity, also known as the true positive rate
#Specificity, also known as the true negative rate
#Es muy sensible si logra detectar positivos que si son positivos. Es muy especifico si
#logra detectar negativos que si son negativos.
#Sensitivity = TP/(TP+FN)
#Specificity = TN/(TN+FP)  O  precision = TP/(TP+FP) 

#Para no tener que calcular todo esto, la funcion confusionMatrix nos lo da todo. Le
#damos los datos a analizar y los factores de referencia. La funcion toma el primer
#factor y analiza los positivos y negativos respecto a este. En nuestro caso el primer
#factor es "Female", porque viene antes en orden alfabetico. Vemos que el modelo es muy
#Especifico porque puede detectar negativos bien (aquellos que no son Female). Pero no es
#muy sensible porque no detecta bien los positivos. La prevalencia de mujeres es baja, de
#22.7%, como se puede ver en prevalence. Accuracy sigue siendo alto.
confusionMatrix(data = y_hat, reference = test_set$sex)

#El promedio de especificidad y sensitividad se llama balanced accuracy. Es utili para 
#tener todo en un mosmo valor. Mejor aun usar el F1, que da un promedio harmonico entre
#especi. y sensiti.
#En algunos casos la sensitividad puede se mas importante que la especificidad o vicever.
#Para estos casos, en el calculo de F1 podemos incluir una beta que pondere que tan 
#importante es una sobre la otra.
#La funcion F_meas calcula el F1. Si no indicamos lo contrario, beta=1. Podemos cambiar
#el codigo que usamos arriba para ver que valor de x maximiza el F1 en lugar de la oa.
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
#Aqui la grafica:
data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()
#Vemos que el maximo es de 61.4% y se alcanza en 66 inches.
max(F_1)
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff
#Como podemos ver, da un mejor balance a sensitividad y especificidad:
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)
#It takes height as a predictor and predicts Female if you are 66 inches or shorter.
#En casos en que sabemos que la prevalencia es muy cercana a 1 o a 0 hay que disenar un
#algoritmo que atienda esto. No nos sirve tener mucha sensibilidad si nuestro algoritmo
#esta tratando de detectar una enfermedad super rara en la que preferimos ser precisios y
#especificos.

#Si volvemos al modelo de solo adivinar y cambiamos la frecuencia en que nuestro algorit
#selecciona Male, nuestra accuracy aumenta. Sin emabrgo, esto disminuye la sensibilidad.
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
#Como tenemos que contemplar tanto sensibilidad como especificidad, es importante ver
#ambas. Una forma de hacer esto es con una grafica llamada ROC que se ve asi
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")
#En el eje x tenemos sensitividad y en el y 1-especificidad. Un algoritmo perfecto
#tendria una linea horizontal hasta arriba, lo que implica sensitividad perfecta para
#todos los niveles de especificidad. En este caso, por ejemplo, del punto 7 al 8 se gana
#mucha sensibilidad, pero del 8 al 9 no tanta.

#Asi se ve si comparamos el adivinar con el algoritmo
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

#Podemos asignar etiquetas a cada valor para ver cual seria el mejor cutoff para nuestro
#algoritmo.
library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01) +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

#El problema de las curvas ROC es que no contemplan prevalencia. Recordemos que en este
#caso la prevalencia de Female es muy baja. Una curva que si contempla prevalencia se
#llama precision-recall plot. Nos muestra la precision frente a los "nombrados":
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
#De aqui podemos ver que la precision no es tan alta como parecia mostrarse en la ROC, 
#esto se debe a que la prevalencia es baja. OJO LAS ESCALAS.
#Si cambiamos el positivo para que sea "Male" la curva ROC se ve igual, pero la precision
#recall si cambia, pues la prevalencia aumenta: OJO LAS ESCALAS.
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#TEMA 2:
#PROBABILIDAD CONDICIONAL: Fueron unicamente ejercicios y teoria.
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0),
                           replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1),
                           replace=TRUE, prob=c(0.15, 0.85))

mean(test)

.15*(.02/(1-0.114509))

mean(disease[test==0])
mean(disease[test==1])/.02

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)


ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)