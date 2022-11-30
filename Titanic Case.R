library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(titanic_clean$Survived, times =1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]
nrow(train_set)
nrow(test_set)

mean(train_set$Survived == 1)

set.seed(3, sample.kind = "Rounding")
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

test_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)
test_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)


sex_model <- ifelse(test_set$Sex == "female", 1, 0)
mean(sex_model == test_set$Survived)


train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))

class_model <- ifelse(test_set$Pclass == 1, 1, 0)
mean(class_model == test_set$Survived)


train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)

sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)


confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))


F_meas(data = factor(sex_model), reference = factor(test_set$Survived))
F_meas(data = factor(class_model), reference = factor(test_set$Survived))
F_meas(data = factor(sex_class_model), reference = factor(test_set$Survived))

train_lda <- train(Survived ~., method = "qda", data = test_set)
y_hat <- predict(train_lda, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]


set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)

set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune
ggplot(train_knn)
knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune
knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$Survived)

set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
train_rpart <- train(Survived ~ .,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
             data = train_set)
train_rpart$bestTune
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)
ggplot(train_rpart)

train_rpart$finalModel
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)

set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
train_rf <- train(Survived ~ .,
                     method = "rf",
                     ntree = 100,
                     tuneGrid = data.frame(mtry = seq(1:7)),
                     data = train_set)
train_rf$bestTune
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived)
imp <- varImp(train_rf)
imp
