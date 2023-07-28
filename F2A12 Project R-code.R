
library(lubridate)
library(tidyverse)
library(caret)
library(data.table)
library(stringr)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)


colnames(movies) <- c("movieId", "title", "genres")
### Split Raw Data: Train and Test Sets
#We now create a validation set of 10% of Movielens data and 90% set for training
#Training dataset used for building the algorithm and the validation set used for testing. 


set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
train <- movielens[-test_index,]
test <- movielens[test_index,]

##We make sure userId and movieId in validation set are also in train set:
validation <- test %>% semi_join(train, by = "movieId") %>% semi_join(train, by = "userId")

##and finnally we add the rows removed from validation set back into train set:
removed <- anti_join(test, validation)
train <- rbind(train, removed)
##we remove unneeded 
rm(dl, ratings, movies, test_index, test, movielens, removed)
train
validation



##Data Exploration & Visualization
#Before we start building the model, we need to get  familiar and understand  the data structure in order to build better model.




head(train)


summary(train)

#No.of unique movies and users 
train %>% summarize(n_users=n_distinct(userId),n_movies=n_distinct(movieId))


#Next we going to check if dataset contains any missing values

any(is.na(train))


## Exploratory Data Analysis (EDA)

summary(train$rating)



ggplot(train,aes(x=rating)) + geom_bar() + labs(title="Distribution of Ratings",x="Rating",y="No.of Ratings")