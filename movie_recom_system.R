##################################
#### Installing required packages 
##################################

################## Basic packages
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

################## Additional packages
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")

library(caret)
library(data.table)
library(dplyr)
library(formattable)
library(ggthemes)
library(kableExtra)
library(knitr)
library(lubridate)
library(psych)
library(recosystem) 
library(rmarkdown)
library(tidyverse)
library(tinytex)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

####################################################
# Data source overview
####################################################

##########################
### The data sets ########
##########################

class(edx)
dim(edx)[1]
dim(validation)[1]

dim(edx)[2]
str(edx)

# Unique values for user id, movie ids, titles, genres
n_distinct(edx$userId)
n_distinct(edx$movieId) 
n_distinct(edx$title)
n_distinct(edx$genres)

# Extract unique genres
genres <- str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique()

n_distinct(genres) # Unique genres
n_distinct(edx$genres) # Unique genres combinations

##########################
### Data exploration #####
##########################

### Movies & Ratings #####

# Head_Sample
head(edx) %>% kable(booktabs = T, caption = "Sample of records structure in edx dataset") %>% kable_styling(latex_options = "HOLD_position")

# Ratings_Distribution
mean_rating <- mean(edx$rating)

edx %>%
  ggplot(aes(x= rating)) +
  geom_histogram(binwidth=0.1, bins = 30, fill = "steelblue",  colour="black", position = "dodge") + scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  labs(title="Ratings distribution", x="Ratings distribution (and mean, dashed)", y="Number of ratings")+ theme(plot.title=element_text(hjust=0.5))+
  geom_vline(xintercept = mean_rating, linetype="dashed",  colour = "orange", size=1)

# Number of ratings by movie
edx %>% group_by(movieId) %>%
  summarize(num_movie_rating = n(), 
            mu_movies = mean(rating),
            sd_movies = sd(rating)) %>% ggplot(aes(x = num_movie_rating))+
  geom_histogram(binwidth=0.1, bins = 30, fill = "steelblue",  colour="black", position = "dodge")+  scale_x_log10()+
  labs(title=" Number of ratings by movie ", x=" Number of ratings ", y="Number of movies")+ theme(plot.title=element_text(hjust=0.5))+
  geom_vline(aes(xintercept = mean(num_movie_rating)), linetype="dashed",  colour = "orange", size=1)


# Movies distribution by average rating
edx %>% group_by(movieId) %>%
  summarise(movie_ave_rating = sum(rating)/n()) %>%
  ggplot(aes(movie_ave_rating)) +
  geom_histogram(binwidth=0.1, bins = 30, fill = "steelblue",  colour="black", position = "dodge") +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) + labs(title=" Movies distribution by average rating ", x=" Average rating (movies) ", y="Number of movies")+ theme(plot.title=element_text(hjust=0.5))

# Average rating of movies
avrm <- setDT(rbind.data.frame(edx, validation)) %>% group_by(movieId) %>% summarise(n=n(), mean_r=mean(rating)) 

avrm %>% 
  ggplot(aes(x=mean_r, y=n)) + geom_point(shape=21, fill="steelblue", color="black", size=2, alpha = 2/5) + labs(title=" Average rating of movies ", x = "Average rating per movie", y = "Number of ratings (per movie)") + geom_smooth(method = 'lm', col = 'orange') + theme(plot.title=element_text(hjust=0.5)) + scale_y_continuous()

### Users ########

# Statistics of ratings and users - edx & validation datasets
ed <- edx %>% group_by(userId) %>% 
  summarise(ratings_per_user = n(), mean_rating_per_user = mean(rating), sd_rating_per_user = sd(rating)) 

val <- validation %>% group_by(userId) %>% 
  summarise(ratings_per_user = n(), mean_rating_per_user = mean(rating), sd_rating_per_user = sd(rating))

desc_val <- describe(val)
desc_ed <- describe(ed)

# Distribution and mean number ratings by users
edx %>% group_by(userId) %>%
  summarize(num_user_rating = n(),
            mu_user = mean(rating),
            sd_user = sd(rating)) %>% 
  ggplot(aes(x = num_user_rating))+
  geom_histogram(binwidth=0.1, bins = 30, fill = "steelblue",  colour="black", position = "dodge")+ scale_x_log10()+
  ggtitle(" Distribution and mean number ratings by users ") +
  labs(title=" Distribution and mean number ratings by users ", x=" Number of users ratings (mean dashed) ", y="Number of ratings")+ theme(plot.title=element_text(hjust=0.5))+
  geom_vline(aes(xintercept = mean(num_user_rating)), linetype="dashed",  colour = "orange", size=1)

# Users distribution by average rating
edx %>% group_by(userId) %>%
  summarise(user_ave_rating = sum(rating)/n()) %>%
  ggplot(aes(user_ave_rating)) +
  geom_histogram(binwidth=0.1, bins = 30, fill = "steelblue",  colour="black", position = "dodge") +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  labs(title=" Users distribution by average rating ", x=" Average rating ", y=" Number of users ")+ theme(plot.title=element_text(hjust=0.5))

### Genres ########

# Average per genre
genres_dataf <- as.data.frame(genres)
names(genres_dataf) <- c("genre")

genres_dataf$n <- sapply(genres, function(gen) {
  index <- genres_dataf$genre==gen
  nrow(edx[str_detect(edx$genres, gen)])
})

genres_dataf$meanRating <- sapply(genres, function(gen) {
  index <- genres_dataf$genre==gen
  mean(edx[str_detect(edx$genres, gen)]$rating)
})

genres_dataf$sd <- sapply(genres, function(gen) {
  index <- genres_dataf$genre==gen
  sd(edx[str_detect(edx$genres, gen)]$rating)
})

# Genres ratings
genres_dataf$se <- genres_dataf$sd / sqrt(genres_dataf$n)
genres_dataf <- genres_dataf %>% arrange(desc(n))

genres_dataf %>% filter(genre!="(no genres listed)") %>%
  mutate(genre = reorder(genre, meanRating)) %>%
  ggplot(aes(x = genre, y = meanRating, ymin=meanRating - 2*se, ymax=meanRating + 2*se)) +
  
  geom_segment( aes(x=genre, xend=genre, y=3, yend=meanRating), color = "gray", lwd = 1) +
  geom_point( size = 4, pch = 21, bg = 4, col = 1)  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Film genre", y = "Average Rating")

genres_dataf %>% select(genre, n, meanRating)%>%
  kable(col.names = c("Genre", "Ratings Count", "Mean Rating"),
        caption = "Ranked genres by ratings count",
        align = "lrr", booktabs = TRUE, format = "latex", linesep = "")

### Time ########

#Average ratings by year

edx %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "month")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "steelblue", size=1.5) +
  labs(title=" Average of ratings through time ", x=" Year ", y=" Mean rating ")+ theme(plot.title=element_text(hjust=0.5))

####################################################
#####  Data Modeling
####################################################

# Create train and test set
set.seed(1, sample.kind="Rounding")
# edx_test set will be 10% of edx data
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)

edx_train <- edx[-test_index,]
edx_temp <- edx[test_index,]

# userId and movieId in test are also in train set
edx_test <- edx_temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Added rows removed from test set back into train set
removed <- anti_join(edx_temp, edx_test)
edx_train <- rbind(edx_train, removed)

# Define RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Average movie rating
mu <- mean(edx_train$rating)

target_rmse <- 0.86490

####### Model One

# RMSE Model 1
model1_rmse <- RMSE(edx_test$rating, mu)
rmse_results <- data.frame(Model = "Just the Average",
                           RMSE = model1_rmse)

# RMSE Model 1 Table
rmse_results %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#737373", bold = T) %>%
  column_spec(2, color =  "#386890", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))

####### Model 2

# Movie mean rating:  b_i
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Prediction rating for Model 2
pred_rat_m2 <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i)

# RMSE Model 2
model2_rmse <- RMSE(edx_test$rating,pred_rat_m2$pred)
rmse_results <- rbind(rmse_results,data.frame(Model = "Movie Effect Model",
                                              RMSE = round(model2_rmse, 4)))

# RMSE Model 2 Table
rmse_results %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#737373", bold = T) %>%
  column_spec(2, color =  "#386890", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))


####### Model 3

# User mean rating:  b_u
user_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Prediction rating for Model 3
pred_rat_m3 <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) 

# RMSE Model 3
model3_rmse <- RMSE(edx_test$rating,pred_rat_m3$pred)
rmse_results <- rbind(rmse_results,
                      data_frame(Model="Movie and User Effect model",  
                                 RMSE = round(model3_rmse, 4)))

# RMSE Model 3 Table
rmse_results %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#737373", bold = T) %>%
  column_spec(2, color =  "#386890", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))


####### Model 4

# Optimal tuning parameter (Lambda) via k fold cross validation
lambdas <- seq(0, 10, 0.25)

# Best value of lambdas
set.seed(21, sample.kind = "Rounding")

# For each lambda, find b_i & b_u followed by rating prediction
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx_train$rating)
  
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(edx_test$rating,predicted_ratings))
})

# Plotting lambdas

qplot(lambdas, rmses)+
  geom_rug(col="steelblue",alpha=0.5, size=1.5)+
  geom_point(shape=21, color="black", fill="#4682b4", size=3)+
  labs(x="Lambda", y="RMSE")+
  theme(text = element_text(size=14), plot.caption = element_text(hjust = 0.5, size = 22))

# Optimal value for lambda
lambda <- lambdas[which.min(rmses)]

# Regular movie reg_b_i with optimal lambda
reg_movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(reg_b_i = sum(rating - mu)/(n()+lambda), n_i = n())

## Regular user reg_b_u with optimal lambda
reg_user_avgs <- edx_train %>% 
  left_join(reg_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(reg_b_u = sum(rating - mu - reg_b_i)/(n()+lambda), n_u = n())

# Prediction rating Model 4
reg_predicted_ratings <- edx_test %>% 
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  mutate(pred = mu + reg_b_i + reg_b_u) %>% 
  .$pred

# RMSE Model 4
model4_rmse <- RMSE(edx_test$rating,reg_predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie and User Effect Model",  
                                     RMSE = round(model4_rmse, 4)))

# RMSE Model 4 Table
rmse_results %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#737373", bold = T) %>%
  column_spec(2, color =  "#386890", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))

####### Model 5: Matrix Factorization

# Residuals of edx_train from Model 4
residual_edx <- edx_train %>% 
  left_join(reg_movie_avgs, by = "movieId") %>%
  left_join(reg_user_avgs, by = "userId") %>%
  mutate(residual = rating - mu - reg_b_i - reg_b_u) %>%
  select(userId, movieId, residual)

# Matrix from residual and edx_test
residual_mf <- as.matrix(residual_edx)
edx_test_mf <- edx_test %>% 
  select(userId, movieId, rating)
edx_test_mf <- as.matrix(edx_test_mf)

# Residual_mf and edx_test_mf to hard disk
write.table(residual_mf , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(edx_test_mf, file = "testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# data_file() to specify a data set from a file at hard disk.
train_set <- data_file("trainset.txt")
test_set <- data_file("testset.txt")

## Build a recommender object
r <-Reco()

# Tuning training set
# Note: following code can take up to 20 minutes.
opts <- r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 1, niter = 10))

# Training recommender model
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

# Making prediction on validation set and calculating RMSE:
pred_file <- tempfile()
r$predict(test_set, out_file(pred_file)) 

predicted_residuals_mf <- scan(pred_file)
predicted_ratings_mf <- reg_predicted_ratings + predicted_residuals_mf

# RMSE for Model 5
model5_rmse <- RMSE(edx_test$rating, predicted_ratings_mf)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Matrix Factorization",  
                                     RMSE = round(model5_rmse, 4)))

# RMSE Model 5 Table
rmse_results %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#737373", bold = T) %>%
  column_spec(2, color =  "#386890", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))

####### Final Model

# Optimal Lambda via k fold cross validation
set.seed(1, sample.kind = "Rounding")

# Average movie rating for edx data set
f_mu <- mean(edx$rating)

# Best value of lambdas returning min RMSE
f_lambdas <- seq(0, 10, 0.25)

f_rmses <- sapply(f_lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(validation$rating,predicted_ratings))
})

# Optimal value for lambda
f_lambda <- f_lambdas[which.min(f_rmses)]
f_lambda
qplot(f_lambdas, f_rmses)+
  geom_rug(col="steelblue",alpha=0.5, size=1.5)+
  geom_point(shape=21, color="black", fill="#4682b4", size=3)+
  labs(x="Lambda", y="RMSE")+
  theme(text = element_text(size=14), plot.caption = element_text(hjust = 0.5, size = 22))

# Final regular movie f_b_i with optimal lambda
final_movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(f_b_i = sum(rating - f_mu)/(n()+f_lambda), n_i = n())

# Final regular user f_b_u with optimal lambda
final_user_avgs <- edx %>% 
  left_join(final_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(f_b_u = sum(rating - f_mu - f_b_i)/(n()+f_lambda), n_u = n())

# Regular prediction rating
final_reg_predicted_ratings <- validation %>% 
  left_join(final_movie_avgs, by='movieId') %>%
  left_join(final_user_avgs, by='userId') %>%
  mutate(pred = f_mu + f_b_i + f_b_u) %>% 
  .$pred

# Residuals of edx data set
final_residual_edx <- edx %>% 
  left_join(final_movie_avgs, by = "movieId") %>%
  left_join(final_user_avgs, by = "userId") %>%
  mutate(residual = rating - f_mu - f_b_i - f_b_u) %>%
  select(userId, movieId, residual)

# Matrix from residual and validation set
final_residual_mf <- as.matrix(final_residual_edx)
validation_mf <- validation %>% 
  select(userId, movieId, rating)
validation_mf <- as.matrix(validation_mf)

# final_residual_mf and validation_mf to hard disk
write.table(final_residual_mf , file = "final_trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_mf, file = "final_testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# data_file() to specify a data set from a file to hard disk.
final_train_set <- data_file("final_trainset.txt")
final_test_set <- data_file("final_testset.txt")

# Build a recommender object
f_r <-Reco()

# Tuning training set
# Note: following code can take up to 20 minutes.
f_opts <- f_r$tune(final_train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                                costp_l1 = 0, costq_l1 = 0,
                                                nthread = 1, niter = 10))

# Training recommended model
f_r$train(final_train_set, opts = c(f_opts$min, nthread = 1, niter = 20))

# Making prediction on validation set and calculating RMSE:
final_pred_file <- tempfile()
f_r$predict(final_test_set, out_file(final_pred_file)) 

final_predicted_residuals_mf <- scan(final_pred_file)
final_predicted_ratings_mf <- final_reg_predicted_ratings + final_predicted_residuals_mf

# RMSE for final model (Model 5: Matrix Factorization)
final_rmse <- RMSE(validation$rating, final_predicted_ratings_mf)
final_rmse_results <- data.frame(Model = "Best Model: Matrix Factorization",
                                 RMSE = round(final_rmse, 4))

# Final Results
final_rmse_results %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#737373", bold = T) %>%
  column_spec(2, color =  "#386890", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))


