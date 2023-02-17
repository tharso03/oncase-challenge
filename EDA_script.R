##### análise exploratória de dados - receitas ######

# importando libraries
library(jsonlite)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(wordcloud)
library(tm)
library(stringr)

## funções auxiliares


# função para transformar listas de caracteres em caracteres.

flatten_lists_of_lists <- function(vec, collapse_char) {
  result <- character(length(vec))
  for (i in seq_along(vec)) {
    result[i] <- paste(unlist(unlist(vec[i])), collapse= collapse_char)
  }
  return(result)
}

### importando e limpando dados

connection <- file("receitas.json")

df <- stream_in(con = connection, simplifyDataFrame = TRUE) %>% 
  as_tibble() %>% 
  mutate(
    directions = flatten_lists_of_lists(directions, " "),
    categories = flatten_lists_of_lists(categories, "; "),
    ingredients = flatten_lists_of_lists(ingredients, "; "),
    date = ymd(substr(date,1,10))
  )

######### análise exploratória de dados

dim(df) # 20.130 observações

colSums(is.na(df))


# date : 19 NAs

summary(df$date)

ggplot(df)+
  geom_histogram(aes(year(date)), binwidth = 1)+
  xlab("Ano")+
  ylab("Frequência absoluta")+
  labs(caption = "19 valores ausentes (0.09% dos dados).")

mean(is.na(df$date))

table(year(df$date)) # até 2003, quase nenhuma receita.

mean(year(df$date)==2004, na.rm = T) # 57% das receitas foram incluídas em 2004



# fat: 4222 NAs, faltou unidade de medida

summary(df$fat)

ggplot(df)+
  geom_histogram(aes(fat), bins = 100)+
  xlim(c(0,200))+
  xlab("Gordura (u.m.)")+
  ylab("Frequência absoluta")+
  labs(caption = "4222 valores ausentes (20.9% dos dados). \n 171 observações com gordura > 200 u.m. (0.1% dos dados).")


mean(is.na(df$fat))

sum(df$fat>200, na.rm = T)

mean(df$fat>200, na.rm = T)

# protein : 4201 NAs

summary(df$protein)

mean(is.na(df$protein))

sum(df$protein>200, na.rm = T)

mean(df$protein>200, na.rm = T)

ggplot(df)+
  geom_histogram(aes(protein), bins = 100)+
  xlim(c(0,200))+
  xlab("Proteína (u.m.)")+
  ylab("Frequência absoluta")+
  labs(caption = "4201 valores ausentes (20.8% dos dados). \n 122 observações com proteína > 200 u.m. (0.7% dos dados).")

# sodium : 4156 NAs

summary(df$sodium)

mean(is.na(df$sodium))

sum(df$sodium>4500, na.rm = T)

mean(df$sodium>4500, na.rm = T)

ggplot(df)+
  geom_histogram(aes(sodium), bins = 100)+
  xlim(c(0,4500))+
  xlab("Sódio (u.m.)")+
  ylab("Frequência absoluta")+
  labs(caption = "4156 valores ausentes (20.6% dos dados). \n 220 observações com sódio > 4500 u.m. (1.3% dos dados).")


# calories : 4154 NAs

summary(df$calories)

mean(is.na(df$calories))

sum(df$calories>3000, na.rm = T)

mean(df$calories>3000, na.rm = T)


ggplot(df)+
  geom_histogram(aes(calories), bins = 100)+
  xlim(c(0,3000))+
  xlab("Calorias (u.m.)")+
  ylab("Frequência absoluta")+
  labs(caption = "4154 valores ausentes (20.6% dos dados). \n 171 observações com calorias > 3000 u.m. (1.3% dos dados).")


# rating: 30 NAs. NA = 0 votos?

summary(df$rating)

mean(is.na(df$rating))

#sum(df$rating>3000, na.rm = T)

#mean(df$rating>3000, na.rm = T)


ggplot(df)+
  geom_histogram(aes(rating), bins = 5)+
  xlab("Avaluação")+
  ylab("Frequência absoluta")+
  labs(caption = "30 valores ausentes (0.1% dos dados).")

# maioria das receitas tem avaliação de 3(incluso) a 4(exlucso) estrelas (33.14%)

cat_word_freq <- table(unlist(strsplit(df$categories, "; ")))

# wordcloud(names(cat_word_freq), freq = cat_word_freq, random.order = F)

# alta frequência: Soy freem vegetarian, pescatarian, Kosher, Gourmet, Dairy Free

# ingredientes

ingredient_word_freq <- table(unlist(strsplit(df$ingredients, "; ")))

#wordcloud(names(ingredient_word_freq), freq = ingredient_word_freq, random.order = F)

# título

corpus <- Corpus(VectorSource(df$title))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)

#wordcloud(corpus, scale = c(10,0.5), random.order = F)

#### high calorie wordcloud

high_cal_df <- df[!is.na(df$calories)&df$calories>quantile(df$calories,0.75,na.rm=T),]

high_cal_corpus <- Corpus(VectorSource(high_cal_df$title))
high_cal_corpus <- tm_map(high_cal_corpus, removeWords, stopwords("english"))
high_cal_corpus <- tm_map(high_cal_corpus, stripWhitespace)
high_cal_corpus <- tm_map(high_cal_corpus, tolower)

#wordcloud(corpus, scale = c(10,0.5), random.order = F)

### five-star recipe wordcloud

five_star_df <- df[!is.na(df$rating)&df$rating==5,]

five_star_corpus <- Corpus(VectorSource(five_star_df$title))
five_star_corpus <- tm_map(five_star_corpus, removeWords, stopwords("english"))
five_star_corpus <- tm_map(five_star_corpus, stripWhitespace)
five_star_corpus <- tm_map(five_star_corpus, tolower)

wordcloud(five_star_corpus, scale = c(10,0.5), random.order = F)

##### categorias das receitas rating = 5

five_star_cat_corpus <- Corpus(VectorSource(high_cal_df$categories))
five_star_cat_corpus <- tm_map(five_star_cat_corpus, removeWords, stopwords("english"))
five_star_cat_corpus <- tm_map(five_star_cat_corpus, stripWhitespace)
five_star_cat_corpus <- tm_map(five_star_cat_corpus, tolower)

wordcloud(five_star_cat_corpus, scale = c(10,0.5), random.order = F)


### is vegetarian, soy-free, dairy-free, kosher

vegetarian_freq <- mean(sapply(df$categories, grepl, pattern = "Vegetarian")) # 34%

kosher_freq <- mean(sapply(df$categories, grepl, pattern = "Kosher")) # 30%

pescat_freq <- mean(sapply(df$categories, grepl, pattern = "Pescatarian")) # 30%

soyfree_freq <- mean(sapply(df$categories, grepl, pattern = "Soy Free")) # 40%

dairyfree_freq <- mean(sapply(df$categories, grepl, pattern = "Dairy Free")) # 15%

restrictions_df <- tibble(
  Restriction = c("Vegetarian", "Kosher", "Pescatarian", "Soy Free", "Dairy Free"),
  Frequency = c(vegetarian_freq, kosher_freq, pescat_freq, soyfree_freq, dairyfree_freq)
)

restrictions_df$Restriction <- factor(restrictions_df$Restriction, levels = restrictions_df$Restriction[order(-restrictions_df$Frequency)])


ggplot(restrictions_df, aes(x = Restriction, y = Frequency)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Frequency*100), "%")), vjust = -0.5) +
  labs(x = "Restrição alimentar", y = "Frequência (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "none")+
  ylim(c(0,0.45))


### dinner, dessert, breakfast

mean(sapply(df$categories, grepl, pattern = "Breakfast")) # 3%

mean(sapply(df$categories, grepl, pattern = "Lunch")) # 6%

mean(sapply(df$categories, grepl, pattern = "Dinner")) # 13%

mean(sapply(df$categories, grepl, pattern = "Dessert")) # 17%

## que variáveis influenciam o rating?

df$vegetarian <- grepl("Vegetarian", df$categories)
df$kosher <- grepl("Kosher", df$categories)
df$pescatarian <- grepl("Pescatarian", df$categories)
df$soyfree <- grepl("Soy Free", df$categories)
df$dairyfree <- grepl("Dairy Free", df$categories)
df$no_restriction <- !(as.logical(df$vegetarian + df$kosher + df$pescatarian + df$soyfree + df$dairyfree))


# Comparando ratings com e sem restrição
ggplot(df, aes(x = vegetarian, y = rating)) +
  geom_boxplot() +
  theme_bw()+
  xlab("Vegetariano")+
  ylab("Avaliação")

ggplot(df, aes(x = kosher, y = rating)) +
  geom_boxplot() +
  theme_bw()+
  xlab("Kosher")+
  ylab("Avaliação")

ggplot(df, aes(x = pescatarian, y = rating)) +
  geom_boxplot() +
  theme_bw()+
  xlab("Pescatarian")+
  ylab("Avaliação")

ggplot(df, aes(x = soyfree, y = rating)) +
  geom_boxplot() +
  theme_bw()+
  xlab("Soyfree")+
  ylab("Avaliação")

ggplot(df, aes(x = dairyfree, y = rating)) +
  geom_boxplot() +
  theme_bw()+
  xlab("Dairy Free")+
  ylab("Avaliação")

ggplot(df, aes(x = no_restriction, y = rating)) +
  geom_boxplot() +
  theme_bw()+
  xlab("Sem restrições alimentares")+
  ylab("Avaliação")

# receitas com restrição alimentar possuem classificação similar àquelas sem restrição

### ano de publicação vs rating

df <- df %>% 
  mutate(year = year(date))

avg_rating <- aggregate(rating ~ year,
                        data = df,
                        FUN = mean)

ggplot(data = avg_rating, aes(x = year, y = rating)) +
  geom_line() +
  geom_point()+
  labs(x = "Ano de publicação", y = "Avaliação média")+
  xlim(c(2004,2016))
####### 

### rating vs quantidade de ingredientes

df <- df %>% 
  mutate(n_ingredients = str_count(ingredients, ";") + 1)


avg_n_ingredients <- aggregate(rating ~ n_ingredients,
                        data = df,
                        FUN = mean)

ggplot(data = avg_n_ingredients, aes(x = n_ingredients, y = rating)) +
  geom_line()+
  geom_point()+
  xlim(c(1,30))+
  labs(x = "Número de ingredientes", y = "Avaliação média")

### rating vs quantidade de palavras no título

df <- df %>% 
  mutate(n_title = str_count(title, " ") + 1)


avg_n_title <- aggregate(rating ~ n_title,
                               data = df,
                               FUN = mean)

ggplot(data = avg_n_title, aes(x = n_title, y = rating)) +
  geom_line()+
  geom_point()+
  xlim(c(0,13))+
  labs(x = "Número de palavras no título", y = "Avaliação média")




### fechar conexão

close(connection)
