### desafio de classificação - oncase ###

### random forest

#### importando libraries
library(readr)
library(tidyr)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)

########## funções auxiliares ##################

precision <- function(model, data) {
  preds <- predict(model, data, type = "response")
  true_positives <- sum(preds == 1 & data$target == 1)
  false_positives <- sum(preds == 1 & data$target == 0)
  if (true_positives + false_positives == 0) {
    return(0)
  } else {
    return(true_positives / (true_positives + false_positives))
  }
}

recall <- function(model, data) {
  preds <- predict(model, data, type = "response")
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



################# importando dados ################

classification_train <- read_csv("classification_data/classification_train.csv") %>% 
  mutate(target = factor(target))

classification_test <- read_csv("classification_data/classification_test.csv") %>% 
  mutate(target = factor(target))


# Fit random forest models to predict target using x1 and x2
rf1 <- randomForest(target ~ x1, data = classification_train)
rf2 <- randomForest(target ~ x2, data = classification_train)

# Create a tibble with precision, recall, and f1-score metrics for the models
metrics <- tibble(
  Model = c("Model 1", "Model 2"),
  Precision = c(precision(model = rf1, data = classification_test), 
                precision(model = rf2, data = classification_test)),
  Recall = c(recall(model = rf1, data = classification_test), 
             recall(model = rf2, data = classification_test)),
  F1_Score = c(F1_Score(model = rf1, data = classification_test), 
               F1_Score(model = rf2, data = classification_test))
)



# Melt the metrics tibble for easier plotting
metrics_melted <- metrics %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# Plot a ggplot barplot to show the models' metrics
ggplot(metrics_melted, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 3)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "", y = "Value", title = "Random Forest Models' Metrics Comparison") +
  scale_fill_manual(values = c("blue", "green")) +
  ylab(c(0,1))+
  theme_classic()

