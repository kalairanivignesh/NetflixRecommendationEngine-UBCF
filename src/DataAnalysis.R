library(stringr)
Ratings <- read.csv("../Data/ratings_group3.csv")
Movies <- read.csv("../Data/movies_group3.csv")
summary(Movies)
summary(Ratings)
separateYearAndTitle <- function()
{
  Movies$title <- str_trim(Movies$title, side=c("right"))
  Movies$Year <<- as.numeric(sub('.*(\\d{4}).*', '\\1', Movies$title))
  Movies$title_new <<- substr(Movies$title, 1 , str_locate(Movies$title,as.character(Movies$Year))-2)
  Movies$title_new[which(is.na(Movies$title_new))] <<- Movies$title[which(is.na(Movies$title_new))]
}
addGenreAsVector <- function()
{
  Movies$Genre_Vector <<-  list(str_split(Movies$genres,"\\|"))
}
separateYearAndTitle()
addGenreAsVector()
write.csv(Movies,"../Data/Movies_NEW.csv")


#Change timestamp to date in ratings.csv
changeTimestampToDate <- function()
{
  Ratings$Date<<- as.POSIXct(Ratings$timestamp, origin = "1970-01-01")  
}
changeTimestampToDate()
write.csv(Ratings,"../Data/Ratings_NEW.csv")


#Merge movies_NEW.csv and Ratings_NEW.csv
MoviesRatings <- merge(Movies,Ratings,by='movieId',all.x=T)
write.csv(MoviesRatings,"../Data/MoviesRatings_NEW.csv")
save(MoviesRatings,file="../Data/MovieRatings.data")
