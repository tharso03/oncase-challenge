##### desafio oncase - classificação #####

#### support vector machine

library(tidyr)
library(dplyr)
library(caret)
library(e1071)
library(ggplot2)

#### auxiliary functions


precision <- function(model, data) {
  preds <- predict(model, newdata = data)
  true_positives <- sum(preds == 1 & data$target == 1)
  false_positives <- sum(preds == 1 & data$target == 0)
  if (true_positives + false_positives == 0) {
    return(0)
  } else {
    return(true_positives / (true_positives + false_positives))
  }
}

recall <- function(model, data) {
  preds <- predict(model, newdata = data)
  true_positives <- sum(preds == 1 & data$target == 1)
  false_negatives <- sum(preds == 0 & data$target == 1)
  if (true_positives + false_negatives == 0) {
    return(0)
  } else {
    return(true_positives / (true_positives + false_negatives))
  }
}

F1_Score <- function(model, data) {
  prec <- precision(model, data)
  rec <- recall(model, data)
  if (prec + rec == 0) {
    return(0)
  } else {
    return(2 * prec * rec / (prec + rec))
  }
}


### importing data

classification_train <- read_csv("classification_data/classification_train.csv") %>% 
  mutate(target = factor(target))

classification_test <- read_csv("classification_data/classification_test.csv") %>% 
  mutate(target = factor(target))

# Fit SVM models to predict target using x1 and x2
#### x1
tune.svm1 <- tune(svm, target ~ x1, data = classification_train, kernel = "linear",
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

summary(tune.svm1)

#svm1 <- svm(x = classification_train[,"x1"], y = classification_train$target, kernel = "linear", scale = FALSE, cost = 0.01)

svm1 <- svm(target ~ x1, data = classification_train, kernel = "linear", scale = FALSE, cost = 0.01)

#### x2
tune.svm2 <- tune(svm, target ~ x2, data = classification_train, kernel = "linear",
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

summary(tune.svm2)

svm2 <- svm(target ~ x2, data = classification_train, kernel = "linear", scale = FALSE, cost = 0.01)

# Predict the target variable for the test dataset using both models
# pred1 <- predict(svm1, newdata = select(classification_test, x1))
# pred2 <- predict(svm2, newdata = select(classification_test, x2))

# Create a tibble with precision, recall, and F1 score metrics for the models

metrics <- tibble(
  Model = c("SVM 1", "SVM 2"),
  Precision = c(precision(model = svm1, data = classification_test), 
                precision(model = svm2, data = classification_test)),
  
  Recall = c(recall(model = svm1, data = classification_test), 
             recall(model = svm2, data = classification_test)),
  
  F1_Score = c(F1_Score(model = svm1, data = classification_test), 
               F1_Score(model = svm2, data = classification_test))
)

# Melt the metrics tibble for easier plotting
metrics_melted <- metrics %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# Plot a ggplot barplot to show the models' metrics
ggplot(metrics_melted, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 3)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "", y = "Value", title = "SVM Models' Metrics Comparison") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_classic()
