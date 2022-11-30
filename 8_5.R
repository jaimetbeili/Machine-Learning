#TEMA 1: CLASIFICACION CON MAS DE DOS CLASES
library(tidyverse)
library(dslabs)
library(caret)
library(rpart)
library(randomForest)
data("olive")
olive %>% as_tibble()
#Para este ejercicio vamos a tratar de predecir de donde viene un aceite dentro de estas
#tres opciones:
table(olive$region)
#Segun sus acidos. Quitamos el area para poder predecirla.
olive <- select(olive, -area)
#Con un modelo KNN tenemos accuracy de .97, muy buena
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)
#El tema aqui es que los predictores son demasiado buenos como para quedarnos con solo .97
#deberiamos de poder llegar a 1. Veamos por ejemplo el predictor "eicosenoic". Solo se
#encuentra en el sur de italia. Y el linoleic claramente separa al norte de italia del 
#resto. Deberiamos de poder predecir de manera perfecta.
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())
#De hecho, podemos ver que si tomamos solo estos dos predictores, los puntos aparecen muy
#claramente separados:
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)
#De esta grafica podemos sacar, visualmente, un arbol de decisiones. Le decimos al
#algoritmo que mire primero el eicosenoic. Si es mayor a .065, predice sur de italia.
#De lo contrario, checa el segundo. Si es mayor a 10.54 predice sardinia. De lo contrario
#predice norte de italia.
#Cuando los datos son continuos el arbol de decision se llama arbol de regresion. Usaremos
#un ejemplo continuo: las encuestas de la eleccion de 2008. Trataremos de predecir el 
#margen dado el dia.
data("polls_2008")
qplot(day, margin, data = polls_2008)
#Para crear un arbol tenemos que partir los datos. Para hacer esto vamos dividiendo en 
#secciones cada vez menores, checando que en cada caso el error estandar sea minimo para
#poder seguir prediciendo. Hacemos esto para cada predictor j, a partir de un valor s. En
#el caso del aceite, se hizo dos veces: una con j = eicosenoic y s = .065 y otra con 
#j = linoleic y s = 10.54
#En este caso solo tenemos un predictor. rpart nos ayuda a dividir:
fit <- rpart(margin ~ ., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
#Acabamos con 8 particiones que se ven asi sobre los datos originales:
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
#Por que se detiene en 8? Si nos fijamos, este proceso podria continuar hasta tener
#literalmente una particion por cada dato. Para evitar eso, existe un parametro de
#complejidad (cp) que limita la minimizacion del error estandar, asegurando que el
#algoritmo no parta tanto y no se sobrenetrene.
#Ademas, existe un minimo de observaciones en las que partir. El default es 20, pero lo
#podemos cambiar con minsplit.
#Finalmente existe minbucket, el minimo de observaciones en cada particion. El default es
#el valor de minsplit/3 redondeado.
#Si ponemos cp en 0 y minsplit en 2, tendriamos una particion por punto:
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
#Podemos podar un arbol para hacerlo mas pequeno si queda muy grande:
pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>% 
  mutate(y_hat = predict(pruned_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#Como escoger un cp adecuado? Lo corssvalidamos:
library(caret)
train_rpart <- train(margin ~ ., method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)

#Este seria el modelo final
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#Cuando el resultado es categorico en lugar de continuo, tenemos arboles de decision.
#La principal diferencia es que en estos casos en lugar de tomar un promedio en cada
#particion y predecir ese promedio, el algoritmo checa cual es la clase que mas se repite
#dentro de una misma particion y predice esa clase. Otra diferencia es que ya no podemos
#usar el error estandar para determinar donde partir, porque los datos son categoricos.
#En su lugar, dos formas de hacer esto son con el indice GINI o la entropia. Ambos buscan
#dividir una particion en particiones "puras". Es decir, que solo tengan una clase.
#Veamos el arbol con el caso del 2 y 7.
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
#Tenemos mejor accuracy que con un modelo logistico, pero menos que con un knn.
#Aunque es muy util este aproach tiene algunos problemas. Principalmente su falta de
#flexibilidad. Los bosques aleatorios pueden ayudar a corregir esto.
#La idea es crear varios arboles de decision de manera aleatoria y promediarlos para crear
#uno mejor. Para crear varios arboles, usamos bootstraping. Creamos varios samples de
#observaciones con replacement y hacemos un arbol para cada uno de estos samples.
fit <- randomForest(margin~., data = polls_2008)
#Aqui podemos ver que mientras mas arboles tenemos, menor es el error:
plot(fit)
#Usando muchos arboles, esta es la prediccion:
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")
#Si lo adaptamos al modelo del 2 y el 7 se ve asi:
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
#Si optimizamos los parametros y usamos un metodo un poco mas rapido (Rborist), podemos
#aumentar la accuracy
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#TEMA 2: CARET
#Lo que el paquete caret nos permite hacer es implementar, con la misma sintaxis, 
#diferentes modelos de machine learning. Por ejemplo, GLM y KNN se podrian entrenar,
#predecir y checar la accuracy de exactamente la misma manera:
data("mnist_27")

train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

#La funcion de train optimiza los parametros necesarios de manera automatica. En el
#caso de KNN el parametro que se optimiza es k:
modelLookup("knn")
#Podemos ver como se optimiza el parametro en una grafica:
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)
#El default es checar k para 5, 7 y 9, pero si queremos buscar uno mejor, podemos usar
#el argumento tunegrid en train:
train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2))) #Importante nombrar k aqui
ggplot(train_knn, highlight = TRUE)
#Cual es el mejor
train_knn$bestTune
#Best performing model:
train_knn$finalModel
#Accuracy en el test:
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

#Si queremos cambiar el metodo de cross validation (optimizacion de parametros), lo
#podemos hacer con trControl (trainControl):
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

#Train tambien calcula las desviaciones estandar, asi que podriamos agregarlas a la
#grafica
train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))
#Asi se ve cuando se compara con la probabilidad condicional. Se puede mejorar todavia un
#poco porque esta muy wiggily la curva:
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

#Podemos usar un modelo mejor conocido como gamLoess. Los parametros a maximizar en este
#modelo son dos:
modelLookup("gamLoess")
#No vamos a cambiar el degree, aunque igual lo tenemos que mencionar. Vamos a hacer un
#grid con varios niveles para span:
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
grid
#Aquie el modelo Loess
train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)
#Tiene accuracy similar al KNN...
confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]
#... Pero mayor smoothnes 
p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

#EJERCICIOS DIFICILES
library(caret)
library(rpart)          
library(dslabs)
# set.seed(1991) # if using R 3.5 or earlier
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)

set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

# set.seed(1991) # if using R 3.5 or earlier
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

imp <- varImp(fit)
imp

tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)



















