### desafio de classificação - oncase ###

### GRADIENT BOOSTING

### importando libraries
library(readr)
library(tidyr)
library(dplyr)
library(caret)
library(xgboost)
library(ggplot2)

###### funções auxiliares ############

precision <- function(model, data) {
  preds <- predict(model, as.matrix(select(data, -target)))
  true_positives <- sum(preds >= 0.5 & data$target == 1)
  false_positives <- sum(preds >= 0.5 & data$target == 0)
  if (true_positives + false_positives == 0) {
    return(0)
  } else {
    return(true_positives / (true_positives + false_positives))
  }
}

recall <- function(model, data) {
  preds <- predict(model, as.matrix(select(data, -target)))
  true_positives <- sum(preds >= 0.5 & data$target == 1)
  false_negatives <- sum(preds < 0.5 & data$target == 1)
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

################# importando dados ################

classification_train <- read_csv("classification_data/classification_train.csv")

classification_test <- read_csv("classification_data/classification_test.csv")

xgb1 <- xgboost(data = as.matrix(select(classification_train, -target, -x2)), 
                label = classification_train$target, 
                nrounds = 100, 
                objective = "binary:logistic")

xgb2 <- xgboost(data = as.matrix(select(classification_train, -target, -x1)), 
                label = classification_train$target, 
                nrounds = 100, 
                objective = "binary:logistic")

# # Predict the target variable for the test dataset using both models
# pred1 <- predict(xgb1, as.matrix(select(classification_test, -target)))
# pred2 <- predict(xgb2, as.matrix(select(classification_test, -target, -x1)))

# Create a tibble with precision, recall, and F1 score metrics for the models
metrics <- tibble(
  Modelo = c("Modelo X1 (XGBoost)", "Modelo X2 (XGBoost)"),
  Precision = c(precision(model = xgb1, data = select(classification_train, -x2)), 
                precision(model = xgb2, data = select(classification_train, -x1))),
  Recall = c(recall(model = xgb1, data = select(classification_train, -x2)), 
             recall(model = xgb2, data = select(classification_train, -x1))),
  F1_Score = c(F1_Score(model = xgb1, data = select(classification_train,-x2)), 
               F1_Score(model = xgb2, data = select(classification_train, -x1)))
)

# Melt the metrics tibble for easier plotting
metrics_melted <- metrics %>%
  pivot_longer(cols = -Modelo, names_to = "Metric", values_to = "Value")

# Plot a ggplot barplot to show the models' metrics
ggplot(metrics_melted, aes(x = Metric, y = Value, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "", y = "Valor") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_classic()+
  ylim(c(0,1))





