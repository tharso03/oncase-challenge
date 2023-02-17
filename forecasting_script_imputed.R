############### desafio oncase - previsão ############

#### importando libraries ####
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(fpp3)
library(forecast)
library(imputeTS)
library(xts)
library(Metrics)
library(reshape2)
##### importando dados ##########


df <- read_excel("time_series_data.xlsx") %>% 
  mutate(nota_data_emissao = ymd(nota_data_emissao),
         produto_descricao = factor(produto_descricao),
         produto_unidade_comercial = factor(produto_unidade_comercial))

# agregando dados por data de venda

df_aggregated <- df %>%
  group_by(produto_descricao, produto_unidade_comercial, nota_data_emissao) %>%
  summarise(produto_quantidade = sum(produto_quantidade))

# visualizando

df_aggregated %>% 
  filter(produto_descricao == "B0") %>% 
  as_tsibble(index = nota_data_emissao) %>% 
  autoplot(produto_quantidade)+
  xlab("Data")+
  ylab("Quantidade de B0")

df_b0 <- df_aggregated %>% 
  filter(produto_descricao == "B0") %>% 
  as_tsibble(index = nota_data_emissao) %>% 
  fill_gaps()

ggplot_na_distribution(df_b0$produto_quantidade)

imp <- na_interpolation(df_b0$produto_quantidade, option = "stine")

ggplot_na_imputations(df_b0$produto_quantidade,imp)

imputed_df <- tibble(
  data = df_b0$nota_data_emissao,
  quantidade = imp
)


# substituindo valores negativos por zero

imputed_df$quantidade <- ifelse(imputed_df$quantidade<0, 0, imputed_df$quantidade)

##### dados semanais

quantity_xts <- xts(imputed_df$quantidade, order.by = imputed_df$data)

weekly_xts <- apply.weekly(quantity_xts, sum)

weekly_df <- as_tibble(weekly_xts) %>% 
  mutate(date = ymd(index(weekly_xts))) %>% 
  rename(quantidade = V1)

### dados mensais

monthly_xts <- apply.monthly(quantity_xts, sum)

monthly_df <- as_tibble(monthly_xts) %>% 
  mutate(date = ymd(index(monthly_xts))) %>% 
  rename(quantidade = V1)


#####

ggplot(imputed_df)+
  geom_line(aes(data,quantidade))+
  xlab("Dia")+
  ylab("Quantidade")

ggplot(weekly_df)+
  geom_line(aes(date,quantidade))+
  xlab("Semana")+
  ylab("Quantidade")

ggplot(monthly_df, aes(date, quantidade))+
  geom_line()+
  xlab("Mês")+
  ylab("Quantidade")

########### MODELAGEM ###########

# criação de dados de treino: todas obs menos 4 últimas

weekly_df_train <- weekly_df %>% 
  slice_head(n = nrow(weekly_df) - 4)

#### modelagem

# Fit an ARIMA model
arima_model <- auto.arima(weekly_df_train$quantidade)
arima_fcst <- forecast(arima_model, h = 4)

# Fit an ETS model
ets_model <- ets(weekly_df_train$quantidade)
ets_fcst <- forecast(ets_model, h = 4)

# Calculate RMSE and MAPE for both models
arima_rmse <- rmse(tail(weekly_df$quantidade,4), arima_fcst$mean)
ets_rmse <- rmse(tail(weekly_df$quantidade,4), ets_fcst$mean)

arima_mape <- mape(tail(weekly_df$quantidade,4), arima_fcst$mean)
ets_mape <- mape(tail(weekly_df$quantidade,4), ets_fcst$mean)

# Create a data frame with the metrics
metrics <- data.frame(model = c("ARIMA", "ETS"),
                      RMSE = c(arima_rmse, ets_rmse),
                      MAPE = c(arima_mape, ets_mape))

# Plot a barplot of the metrics

ggplot(metrics, aes(x = model, y = RMSE, fill = model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(RMSE, 2)), vjust = -0.5) +
  labs(x = "Modelo", y = "RMSE")+
  theme_minimal()

ggplot(metrics, aes(x = model, y = MAPE, fill = model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(MAPE, 2)), vjust = -0.5) +
  labs(x = "Modelo", y = "MAPE") +
  ylim(c(0,1))+
  theme_minimal()


forecast_df <- tibble(
  date = weekly_df$date,
  actual = weekly_df$quantidade,
  forecasted = c(weekly_df_train$quantidade, arima_fcst$mean),
  Hi80 = c(weekly_df_train$quantidade, arima_fcst$upper[,1]),
  Lo80 = c(weekly_df_train$quantidade, arima_fcst$lower[,1])
)

ggplot(forecast_df)+
  geom_line(aes(date,forecasted), color = "blue")+
  geom_line(aes(date,Hi80), color = "blue", linetype = "dashed")+
  geom_line(aes(date,Lo80), color = "blue", linetype = "dashed")+
  geom_line(aes(date,actual), color = "black")+
  xlab("Semana")+
  ylab("Quantidade de B0")+
  scale_x_date(limits = c(ymd("2021-04-01"), ymd("2021-09-30")))













