rm(list = ls())
library(dplyr)
library(tibble)
library(purrr)

# Leer la data ------------------------------------------------------------

df <- read.csv("datasets/toy_dataset.csv", row.names = 1)

# Reemplazo na's por ceros
df[is.na(df)] <- 0


# Estandarizo -------------------------------------------------------------

standarize <- function(x) {
  return((x-mean(x))/(max(x)-min(x)))
}

df <- df %>% 
  rownames_to_column("user") %>% 
  mutate_if(is.numeric,standarize) %>% 
  column_to_rownames("user")


# Matriz de similitud de item ---------------------------------------------

# cos.sim <- function(mat){
#   sim <- mat/sqrt(rowSums(mat*mat))
#   sim <- sim %*% t(sim)
#   return(sim)
# }
# item_sim <- cos.sim(as.matrix(t(df))) %>% as.data.frame()

item_sim <- cor(df) %>% as.data.frame()

# Funcion para calcular pelicula cercana ----------------------------------

get_similar_movies <- function(movie_name, user_rating) {
  similar_score <- item_sim[movie_name]*(user_rating)
  # similar_score <- similar_score[order(similar_score, decreasing = T),, drop = F]
  return(similar_score)
}

# Ejemplo -----------------------------------------------------------------

get_similar_movies("action1",5)

user_movies <- c("action1","romantic2","romantic3")
user_ratings <- c(5,1,1)

user_similar_movies <- map2_dfc(user_movies, user_ratings, get_similar_movies)
rowSums(user_similar_movies) %>% sort(decreasing = T)

