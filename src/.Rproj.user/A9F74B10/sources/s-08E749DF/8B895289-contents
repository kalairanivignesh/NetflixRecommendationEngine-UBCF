
library(dplyr)
library(data.table)
library(ggplot2)

load("../Data/MovieRatings.data")
MoviesRatings <- setDT(MoviesRatings)
#Latest and oldest year movie in database
LatestMovie <- max(MoviesRatings[!is.na(Year),Year])
OldestMovie <- min(MoviesRatings[!is.na(Year),Year])

#Top 5 rated movies
AverageRating <- mean(MoviesRatings[!is.na(rating),rating])

NoOfRatingByUser <- group_by(MoviesRatings[!is.na(userId)],userId) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

head(NoOfRatingByUser)                    
hist(log10(NoOfRatingByUser$count),
     main="Number of movies rated by user",
     col="lightblue")
#Distribution of ratings

as.factor(MoviesRatings$rating)

hist(MoviesRatings$rating,breaks=seq(.4,5.4,.5)
     ,main="Distribution of ratings",
     col="darkmagenta")


#Exploring similarity data

### Recommendation Engine ##
library(data.table)

### Actual Data ##


MovieRating_Full <- setDT(read.csv("../Data/MoviesRatings_New.csv"))
MovieRating_Triple <- MovieRating_Full[,.(userId,movieId,rating)][!is.na(userId)]
#MovieRating_Triplets_1 <- MovieRating_Triplets[userId == "1"]


##MovieRating_Triplets
MovieRatingMatrix <- as(MovieRating_Triplets, "realRatingMatrix")


#To see the content of realRatingMatrix[1:10] in matrix and in dataframe
as(MovieRatingMatrix[1:10], "matrix")

as(MovieRatingMatrix, "data.frame")

## get some information
memory.limit(100000)
library(recommenderlab)
dimnames(MovieRatingMatrix)
rowCounts(MovieRatingMatrix) ## number of ratings per user
colCounts(MovieRatingMatrix) ## number of ratings per movie
colMeans(MovieRatingMatrix) ## average item rating
nratings(MovieRatingMatrix) ## total number of ratings
hasRating(MovieRatingMatrix) ## user-movie combinations with ratings



## inspect a subset
image(MovieRatingMatrix[1:10000],)

#create similarity matrix
similarity_users <- similarity(MovieRatingMatrix, 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
memory.limit(100000)
image(as.matrix(similarity_users), main = "User similarity")
