# Author : Roja Kasula
# Date : March 01,2022 .

# Project : Movie Lense 
# Initial code supplied by edx .
# Create edx set, validation set 
# Note: this process could take a couple of minutes


# Install Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# Load Libraries
library(tidyverse)
library(caret)
library(data.table)
library(hexbin)
library(ggplot2)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Check for download
datafile <- "MovieLens.RData"
if(!file.exists("MovieLens.RData"))
{
  print("Download")
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  
  set.seed(1, sample.kind = "Rounding") # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in validation set are also in edx set
  
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  save(edx, validation, file = datafile)

} else {
  load(datafile)
}

# Data Analysis
# Summary of the  Data

head(edx, 5)
summary(edx)

# display unique no.of movies, Users and Genres on edx data set
edx %>% summarise(
  uniq_movies = n_distinct(movieId),
  uniq_users = n_distinct(userId),
  uniq_genres = n_distinct(genres))

# compute Ratings Mean on edx data set
mean(edx$rating)

# Ratings Histogram on edx data set
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "black",bins=30) +
  xlab("Rating") +
  ylab("Count") +
  ggtitle("Ratings Histogram") +
  theme(plot.title = element_text(hjust = 0.5))

# Ratings Users - no.of Ratings on edx data set
edx %>% 
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black",bins=30) +
  scale_x_log10() +
  xlab("# Ratings") +
  ylab("# Users") +
  ggtitle("Users no.of Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

# Ratings Users - Mean on edx data set
edx %>%
  group_by(userId) %>%
  summarise(mu_user = mean(rating)) %>%
  ggplot(aes(mu_user)) +
  geom_histogram(color = "black",bins=30) +
  ggtitle("Average Ratings per User Histogram") +
  xlab("Average Rating") +
  ylab("# User") +
  theme(plot.title = element_text(hjust = 0.5))

# Ratings Users - Mean by Number using linear method
edx %>%
  group_by(userId) %>%
  summarise(mu_user = mean(rating), number = n()) %>%
  ggplot(aes(x = mu_user, y = number)) +
  geom_point( ) +
  scale_y_log10() +
  geom_smooth(formula= y~x,method = "lm") +
  ggtitle("Users Average Ratings per no. of Rated Movies") +
  xlab("Average Rating") +
  ylab("# Movies Rated") +
  theme(plot.title = element_text(hjust = 0.5))

edx %>%
  group_by(userId) %>%
  summarise(mu_user = mean(rating), number = n()) %>%
  ggplot(aes(x = mu_user, y = number)) +
  geom_bin2d( ) +
  scale_fill_gradientn(colors = grey.colors(10)) +
  labs(fill="User Count") +
  scale_y_log10() +
  geom_smooth(formula= y~x,method = "lm") +
  ggtitle("Users Average Ratings per no. of Rated Movies") +
  xlab("Average Rating") +
  ylab("# Movies Rated") +
  theme(plot.title = element_text(hjust = 0.5))


# RMSE : The Root Mean Square Error 

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## Simple Prediction based on Mean Rating.
mu <- mean(edx$rating)
mu

# compute RMSE for validation and edx and Naive baseline model taking into consideration .
rmse_naive <- RMSE(validation$rating, mu)
rmse_naive

## display output in data frame.
rmse_output = data_frame(method = "Naive Baseline Mean Model", RMSE = rmse_naive)

## Movie Effects Model.
## compute the movie effects, b_i.

mu <- mean(edx$rating)
mu


movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

predicted_ratings <- mu + validation %>%
   left_join(movie_avgs, by='movieId') %>%
   pull(b_i)

rmse_model_movie_effects <- RMSE(predicted_ratings, validation$rating)
rmse_model_movie_effects
rmse_output <- bind_rows(rmse_output, 
                          data.frame(method="Movie Effects Model",
                          RMSE = rmse_model_movie_effects))

## User Effects Model
## Naive model taking into consideration the user effects, b_u
user_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

predicted_ratings <- validation %>%
 left_join(movie_avgs, by='movieId') %>%
 left_join(user_avgs, by='userId') %>%
 mutate(pred = mu + b_i + b_u) %>%
 pull(pred)

rmse_model_user_effects <- RMSE(predicted_ratings, validation$rating)
rmse_model_user_effects
rmse_output <- bind_rows(rmse_output,data.frame(method="User Effects Model",RMSE = rmse_model_user_effects))


## Regularization Model

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n() +l))
  
  b_u <- edx %>%
     left_join(b_i, by="movieId") %>%
     group_by(userId) %>%
     summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>%
   left_join(b_i, by = "movieId") %>%
   left_join(b_u, by = "userId") %>%
   mutate(pred = mu + b_i + b_u) %>%
   pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
  
})
rmse_reg <- min(rmses)
rmse_reg

# Plot RMSE against Lambdas to find optimal lambda
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda

rmse_output <- bind_rows(rmse_output, 
                         data.frame(method="Regularization Model",
                         RMSE = rmse_model_user_effects))

## Final output
rmse_output


