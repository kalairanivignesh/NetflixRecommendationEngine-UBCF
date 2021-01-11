### Recommendation Engine ##
library(recommenderlab)
library(data.table)
library(dplyr)


### Actual Data ##
MovieRating_Full <- setDT(read.csv("../Data/MoviesRatings_New.csv"))

## Creating Triplets to calculate Movie Rating Matrix
MovieRating_Triplets <- MovieRating_Full[,.(userId,movieId,rating)][!is.na(userId)]


# Remove users who gave only one rating


Movies_MoreThanFiveRating <- MovieRating_Triplets %>% group_by(movieId) %>% summarize(n=n()) %>% filter(n>5)


MovieRating_Triplets <- merge(Movies_MoreThanFiveRating,MovieRating_Triplets)[,c("userId","movieId","rating")]

Users_MoreThanOneRating <- MovieRating_Triplets %>% group_by(userId) %>% summarize(n=n()) %>% filter(n>1)


MovieRating_Triplets <- merge(Users_MoreThanOneRating,MovieRating_Triplets)[,c("userId","movieId","rating")]



#MovieRating_Triplets_Five <- merge(Users_MoreThanFiveRating,MovieRating_Triplets)[,c("userId","movieId","rating")]
#MovieRating_Triplets_Five$movieId <- as.character(MovieRating_Triplets_Five$movieId) 


#MovieRating_Triplets_1 <- MovieRating_Triplets[userId == "1"]

MovieRating_Triplets_GROUP <- MovieRating_Triplets %>%
  group_by(userId) %>%
  summarize(n=n()) %>% arrange(n)

#tail(MovieRating_Triplets_GROUP)
##MovieRating_Triplets
MovieRatingMatrix <- as(MovieRating_Triplets, "realRatingMatrix")
#MovieRatingMatrix_MoreThan5Observiation <- as(MovieRating_Triplets_Five, "realRatingMatrix")


#To see the content of realRatingMatrix[1:10] in matrix and in dataframe
as(MovieRatingMatrix[1:100,1:5], "matrix")

as(MovieRatingMatrix[1:100], "data.frame")

## Preparing train and test data
set.seed(1)  # to have same results for random generation


#realRatingMatrix normalized data,not normalizing the data using below code
# normalize_MovieRatingMatrix <- normalize(MovieRatingMatrix)
# 
# normalize_MovieRatingMatrix
# 
# getRatingMatrix(normalize_MovieRatingMatrix)
# 
# image(MovieRatingMatrix, main = "Raw Ratings")


#spliting ratingmatrix into two with the proportion of 30% and 70% 

which_train <- sample(x = c(TRUE, FALSE), size =
                        nrow(MovieRatingMatrix),replace = TRUE, prob = c(0.3,0.7))

which_train

MovieRatingMatrix_Train <- MovieRatingMatrix[which_train]
MovieRatingMatrix_Test <- MovieRatingMatrix[!which_train]


# MovieRatingMatrix_MoreThan5Observiation_Train <- MovieRatingMatrix_MoreThan5Observiation[which_train]
# MovieRatingMatrix_MoreThan5Observiation_Test <- MovieRatingMatrix_MoreThan5Observiation[!which_train]

#To know how many models are there in Recommenderlab
recommender_models <- recommenderRegistry$get_entries(dataType =
                                                        "realRatingMatrix")
recommender_models
memory.limit(200000) # To store memory consuming variables in harddisk, given the limit [> RAM Size] as 100GB else we get memory issues
#Building a model
MovieRating_Recommender_Model <- Recommender(data=MovieRatingMatrix_Train,method="UBCF", 
                                             param=list(normalize = "Z-score",method="Cosine"))

# MovieRating_5_Recommender_Model <- Recommender(data=MovieRatingMatrix_Train,method="UBCF", 
#                                              param=list(normalize = "Z-score",method="Cosine"))
n_recommended <- 5 # generate 5 recommendation per user


#get accurate result if the below code is used for prediction,need huge memory 
#MovieRecommendation_Predicted <- predict(object = MovieRating_Recommender_Model,newdata =
#                                           MovieRatingMatrix, n = n_recommended)  # Prediction for test data

#predict recommendation for the first user
MovieRecommendation_Predicted <- predict(object = MovieRating_Recommender_Model,
                                         newdata=MovieRatingMatrix["1"] , n = n_recommended)  # Prediction for first user test data

save(MovieRating_Recommender_Model,file="../Data/Movie_Recommender_Model.data")
save(MovieRatingMatrix,file="../Data/MovieRatingMatrix.data")
save(MovieRating_Triplets,file="../Data/MovieRating_Triplets.data")

#to see the list of movies recommended for the first user
recom_list <- as(MovieRecommendation_Predicted, 
                 "list")
recom_list


#Example to find prediction based on UserId
SortedUserList <- unique(sort(MovieRating_Triplets$userId))
which(SortedUserList == 1116)


MovieRecommendation_Predicted <- predict(object = MovieRating_Recommender_Model,newdata =
                                           MovieRatingMatrix[which(SortedUserList == 4405)], n = n_recommended)  # Prediction for test data

recom_list <- as(MovieRecommendation_Predicted, 
                 "list")
recom_list



### Evaluating the Recommendation Model ###


n_fold <- 5  # 5-fold
eval_sets <- evaluationScheme(data=MovieRatingMatrix_Train[1:10000],
                              method = "split", 
                              given=2,
                              goodRating=3
                              )

eval_sets


size_sets <- sapply(eval_sets@runsTrain, length)
size_sets


eval_results <- evaluate(x = eval_sets, method = "UBCF", n = 5)


model_to_evaluate <- "UBCF"
model_params <- list(method = "cosine",
                     sample = FALSE, # already did this.
                     normalize = "center")

eval_recommender <- Recommender(data = normalize(getData(eval_sets,
                                               "train")),method = "UBCF", parameter = model_params)


eval_recommender



items_to_recommend <- 5

eval_prediction <- predict(object = eval_recommender, newdata
                           =getData(eval_sets,
                                    "known"), n = items_to_recommend,
                           type="ratings")


eval_accuracy_BY_USER <- calcPredictionAccuracy( x = eval_prediction, data
                                         = getData(eval_sets, "unknown"), byUser = TRUE)

eval_accuracy_BY_USER[is.nan(eval_accuracy_BY_USER)]=0

head(eval_accuracy_BY_USER)


eval_accuracy <- calcPredictionAccuracy( x = eval_prediction, data
                                                 = getData(eval_sets, "unknown"), byUser = FALSE)
eval_accuracy


results <- evaluate(x = eval_sets, method = model_to_evaluate, n =
                      seq(10, 100, 10))

head(getConfusionMatrix(results)[[1]])

columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[,
                                                           columns_to_sum]
head(indices_summed)
