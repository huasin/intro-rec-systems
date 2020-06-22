rm(list = ls())
library(data.table)
library(dplyr)

# Leer la data ------------------------------------------------------------

ratings <- fread("datasets/input/ratings.csv")
movies  <- fread("datasets/input/movies.csv")

head(ratings)
head(movies)

# Me quedo con las peliculas que tengan al menos 10 ratings ---------------

ratings[, n_ratings := .N, by=movieId][n_ratings > 10]

# Join y select de la data ------------------------------------------------

setkey(ratings, "movieId")
setkey(movies , "movieId")

df <- movies[ratings]
df <- df[,c("movieId","title","userId","rating")]

# Pivot user ~ movie ------------------------------------------------------

user_item <- dcast(df, userId ~ title, fun = sum, value.var = "rating")
user_item[,userId:=NULL]

# Similarity item matrix --------------------------------------------------

item_sim <- cor(user_item, method = "pearson")
i <- data.frame(item_sim)

# Calcular ejemplo --------------------------------------------------------

get_similar_movies <- function(movie_name, user_rating) {
  similar_score <- item_sim[movie_name]*(user_rating-2.5)
  # similar_score <- similar_score[order(similar_score, decreasing = T),, drop = F]
  return(similar_score)
}

get_similar_movies("(500) Days of Summer (2009)",1)


user_movies <- c("2 Fast 2 Furious (Fast and the Furious 2, The) (2003)","12 Years a Slave (2013)","2012 (2009)","(500) Days of Summer (2009)")
user_ratings <- c(5,4,3,2)

user_similar_movies <- purrr::map2_dfr(user_movies, user_ratings, get_similar_movies)
rowSums(user_similar_movies) %>% sort(decreasing = T)
