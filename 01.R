library(recommenderlab)
library(dplyr)
library(data.table)

df <- fread("datasets/input/ratings.csv")
df[,timestamp:=NULL]

r <- df %>% as("realRatingMatrix")
getRatingMatrix(r[1:10,1:50])

r[1:10,1:10] %>% as("matrix")
r %>% as("list")
r %>% as("data.frame") %>% head


r %>% getRatings %>% hist
normalize(r, method = "center", row = T) %>% getRatings %>% hist

rec <- Recommender(r,"SVD")
predict(rec,r[1]) %>% as("list")




scheme <- evaluationScheme(r, method = "split", train = .8, given = 5, goodRating = 4)
train <- getData(scheme, "train")
known <- getData(scheme, "known")
unknown <- getData(scheme, "unknown")


param = list()
param$ubcf <- list(method = "cosine",
                   nn = 25,
                   sample = FALSE,
                   weighted = TRUE,
                   normalize = "center",
                   min_matching_items = 0,
                   min_predictive_items = 0)

param$ibcf <- list(k = 30,
                   method = "Cosine",
                   normalize = "center",
                   normalize_sim_matrix = FALSE,
                   alpha = .5,
                   na_as_zero = FALSE)

r3 <- Recommender(train, method="UBCF", list(method = "Pearson", nn))


r1 <- Recommender(train, method="SVD", list(k = 10))
r2 <- Recommender(train, method="ALS", list(n_factors = 10, n_iterations = 30, normalize = "center"))
r4 <- Recommender(train, method="IBCf", list(k = 30))
r5 <- Recommender(train, method="AR", list(k = 40))

p1 <- predict(r1, known, type="ratings")
p2 <- predict(r2, known, type="ratings")
p3 <- predict(r3, known, type="ratings")
p4 <- predict(r4, known, type="ratings")

error <- rbind(calcPredictionAccuracy(p1, unknown),
               calcPredictionAccuracy(p2, unknown),
               calcPredictionAccuracy(p3, unknown),
               calcPredictionAccuracy(p4, unknown))

error
