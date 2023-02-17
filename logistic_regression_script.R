### desafio de classificação - oncase ###

#### importando libraries
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(tidyr)

##### funções auxiliares

precision <- function(model, data) {
  preds <- predict(model, data, type = "response")
  true_positives <- sum(preds >= 0.5 & data$target == 1)
  false_positives <- sum(preds >= 0.5 & data$target == 0)
  if (true_positives + false_positives == 0) {
    return(0)
  } else {
    return(true_positives / (true_positives + false_positives))
  }
}

recall <- function(model, data) {
  preds <- predict(model, data, type = "response")
  true_positives <- sum(preds >= 0.5 & data$target == 1)
  false_negatives <- sum(preds < 0.5 & data$target == 1)
  if (true_positives + false_negatives == 0) {
    return(0)
  } else {
    return(true_positives / (true_positives + false_negatives))
  }
}

F1_score <- function(model, data) {
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

#### EDA ####

ggplot(classification_train, aes(x = x1, y = x2, color = target)) + 
  geom_point() +
  labs(x = "x1", y = "x2", color = "target")


ggplot(classification_train)+
  geom_boxplot(aes(target,x1))

ggplot(classification_train)+
  geom_boxplot(aes(target,x2))

### modelando

model_x1 <- glm(target ~ x1, data = classification_train, family = "binomial")
model_x2 <- glm(target ~ x2, data = classification_train, family = "binomial")

### comparando modelos


# Calculate evaluation metrics for models using new data
eval_tbl <- data.frame(
  model = c("x1", "x2"),
  precision = c(precision(model_x1, classification_test), precision(model_x2, classification_test)),
  recall = c(recall(model_x1, classification_test), recall(model_x2, classification_test)),
  f1 = c(F1_score(model_x1, classification_test), F1_score(model_x2, classification_test))
)

# Plot barplot with model evaluation metrics
eval_tbl %>% 
  pivot_longer(cols = precision:f1, names_to = "Metrica", values_to = "value") %>% 
  ggplot(aes(x = model, y = value, fill = Metrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value, 2)), position = position_dodge(width = 1), vjust = -0.5) +
  labs(x = "Modelo", y = "Valor", fill = "Metrica") +
  theme_classic()+
  ylim(c(0,0.85))




