### desafio de regressão - oncase ###

### importando pacotes
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(GGally)
library(tidyr)
library(leaps)


### importando dados

regression_train <- read_csv("regression_data/regression_train.csv") %>% # 31% das observações têm target = NA
  mutate(X6 = factor(X6)) %>%  # a variável X6 só assume 3 valores: 0, 5 e 8
  drop_na(target)

regression_test <- read_csv("regression_data/regression_test.csv") %>% # 31% das observaçõe stambém têm target = NA
  mutate(X6 = factor(X6)) %>% 
  drop_na(target)

colSums(is.na(regression_train))

colSums(is.na(regression_test))

#### análise exploratória de dados

ggpairs(regression_train)

ggplot(data = regression_train)+
  geom_boxplot(aes(X6,target))

# x2 e x7 têm correlação muito alta com target, mas também têm correlação muito alta entre si mesmas.
# com exceção de uma observação, x7 = x2 * 2.35



########### modelagem #################

fitx7sqrt <- lm(sqrt(target) ~ X7,
            data = regression_train) # modelo bom. cumpre as suposições da regressão linear.


summary(fitx7sqrt)

fs_fit <- regsubsets(sqrt(target) ~ . -X2, data = regression_train, nbest = 1, method = "forward")
summary(fs_fit)

fitx7x1sqrt <- lm(sqrt(target) ~ X7 + X1,
              data = regression_train)

summary(fitx7x1sqrt)

# vale a pena colocar mais variáveis nos modelo? Há um aumento de 3% no R2.


################### comparando os modelos #################

# criar função para calcular métricas

calculate_metrics <- function(model, target, predicted) {
  rmse <- sqrt(mean((target - predicted)^2, na.rm = T))
  r_squared <- summary(model)$r.squared
  corr <- cor(target, predicted, use = "pairwise.complete.obs")
  
  return(c(rmse, r_squared, corr))
}

# calcular métricas para cada modelo
metrics_fitx7sqrt <- calculate_metrics(fitx7sqrt, regression_test$target, predict(fitx7sqrt, regression_test)^2)
metrics_fitx7x1sqrt <- calculate_metrics(fitx7x1sqrt, regression_test$target, predict(fitx7x1sqrt, regression_test)^2)

# juntar métricas em um dataframe
metrics_df <- data.frame(model = c("fitx7sqrt", "fitx7x1sqrt"),
                         RMSE = c(metrics_fitx7sqrt[1], metrics_fitx7x1sqrt[1]),
                         R_squared = c(metrics_fitx7sqrt[2], metrics_fitx7x1sqrt[2]),
                         Correlation = c(metrics_fitx7sqrt[3], metrics_fitx7x1sqrt[3]))


metrics_df # visualizar dataframe

plot(fitx7sqrt)



