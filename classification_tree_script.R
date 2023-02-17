### desafio de classificação - oncase ###

#### importando libraries
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(rpart)
library(rpart.plot)

################# importando dados ################

classification_train <- read_csv("classification_data/classification_train.csv") %>% 
  mutate(target = factor(target))

classification_test <- read_csv("classification_data/classification_test.csv") %>% 
  mutate(target = factor(target))

model_x1 <- train(target ~ x1, data = classification_train, method = "rpart", metric = "Accuracy")
model_x2 <- train(target ~ x2, data = classification_train, method = "rpart", metric = "Accuracy")


predicted_x1 <- predict(model_x1, newdata = classification_test)
predicted_x2 <- predict(model_x2, newdata = classification_test)

conf_x1 <- confusionMatrix(classification_test$target, predicted_x1)
precision_x1 <- conf_x1$byClass[1]
recall_x1 <- conf_x1$byClass[2]
f1_x1 <- conf_x1$byClass[3]

# Calculate precision, recall, and F1-score for model_x2
conf_x2 <- confusionMatrix(classification_test$target, predicted_x2)
precision_x2 <- conf_x2$byClass[1]
recall_x2 <- conf_x2$byClass[2]
f1_x2 <- conf_x2$byClass[3]

# Create a tibble with the results
results <- tibble(Model = c("x1", "x2"),
                  Precision = c(precision_x1, precision_x2),
                  Recall = c(recall_x1, recall_x2),
                  F1_score = c(f1_x1, f1_x2))

# Print the tibble
print(results)




