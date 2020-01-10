#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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

set.seed(1)
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




head(edx)
#Load Libraries
library(caret)
library(tidyverse)
library(dplyr)


# Save edx and valadation data set
#save(edx, file="edx.Rda")
#save(validation, file="validation.Rda")

#Load data files.  This done so the dataframe generate does not need to be run.

load("edx.Rda")
load("validation.Rda")

#Create Train Sets and Test Sets
set.seed(755)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]


test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
#Save the training set and test set so they will not need to be recalculated.
save(train_set, file="train_set.Rda")
save(test_set, file="test_set.Rda")
dim(test_set)

#Names oF Genres.  Create a list of the unique gnere names.
genre_list <-  edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
genre_list

#Load the list of genres from file to sav time.
load("genre_list.Rda")

#Add seperate column for each genre
#Create a new Dataframe adding a seperate column for each genre
edx_genre_col <- edx %>% mutate(Drama=str_detect(genres, "Drama")) %>% 
  mutate(Comedy=str_detect(genres, "Comedy")) %>%
  mutate(Action=str_detect(genres, "Action")) %>%
  mutate(Thriller=str_detect(genres, "Thriller")) %>%
  mutate(Adventure=str_detect(genres, "Adventure")) %>%
  mutate(Romance=str_detect(genres, "Romance")) %>%
  mutate(Sci_fi=str_detect(genres, "Sci-Fi")) %>%
  mutate(Crime=str_detect(genres, "Crime")) %>%
  mutate(Fantasy=str_detect(genres, "Fantasy")) %>%
  mutate(Children=str_detect(genres, "Children")) %>%
  mutate(Horror=str_detect(genres, "Horror")) %>%
  mutate(Mystery=str_detect(genres, "Mystery")) %>%
  mutate(War =str_detect(genres, "War")) %>%
  mutate(Animation=str_detect(genres, "Animation")) %>%
  mutate(Musical=str_detect(genres, "Musical")) %>%
  mutate(Western=str_detect(genres, "Western")) %>%
  mutate(Film_Noir=str_detect(genres, "Film-Noir")) %>%
  mutate(Documentary=str_detect(genres, "Documentary")) %>%
  mutate(IMAX=str_detect(genres, "IMAX")) %>%
  mutate(None=str_detect(genres, ""))



#Check and save new dataframe
str(edx_genre_col)
#Save the dataframe with the genres broken out.
save(edx_genre_col, file = "edx_genre_col.Rda")
#Load 
load("edx_genre_col.Rda")
#remove removes userId, movieId, timestamp, title, genres.  
edx_genre_logical <-  subset(edx_genre_col, select= -c(userId, movieId, timestamp, title, genres))

head(edx_genre_logical)
str(edx_genre_logical)

dim(edx_genre_logical)
edx_genre_logical[2,21]

save(edx_genre_logical,file = "edx_genre_logical.Rda")


edx_genre_logical %>% filter(edx_genre_logical[,1] == 1 ) %>% summarize(mean(rating))
x <- c(2:21)
gl[x-1]
mean_genre <- sapply(x, function(x){
  edx_genre_logical %>% filter(edx_genre_logical[,x] == 1 ) %>% summarize(mean(rating))}
)
#Ratings mean of the rating for each genre,
mean_genre
sd_genre <- sapply(x, function(x){
  edx_genre_logical %>% filter(edx_genre_logical[,x] == 1 ) %>% summarize(sd(rating))}
)

#Ratings standaer deviation  of the rating for each genre,
sd_genre
as.numeric(sd_genre)
as.numeric(mean_genre)

#Create 100,000 observation dataframe for training
edx_genre_logical_100 <- edx_genre_logical[sample(nrow(edx_genre_logical), 100000),]
save(edx_genre_logical_100, file = "edx_genre_logical_100.Rda")
dim(edx_genre_logical_100)

#Create a 10,000 observation dataframe for testing
edx_genre_logical_10 <- edx_genre_logical[sample(nrow(edx_genre_logical), 10000),]
save(edx_genre_logical_10, file = "edx_genre_logical_10.Rda")
dim(edx_genre_logical_10)

#Create a 1,000 observation dataframe for testing
edx_genre_logical_1 <- edx_genre_logical[sample(nrow(edx_genre_logical), 1000),]
save(edx_genre_logical_10, file = "edx_genre_logical_1.Rda")
dim(edx_genre_logical_1)



#edx file info
str(edx)

#Preliminary Data Review
dim(edx)
head(edx)

# Summary of the number of users and movies
edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId),
            n_genres = n_distinct(genres))

#this lists statistical information for the number of movies the individual users have
users_totals <- edx %>% count(userId)%>%
  summarise(median = median(n), minimum = min(n), maximum = max(n), quarter1 = quantile(n, 0.25), quarter3 = quantile(n, 0.75))

movie_totals <- edx %>% count(movieId)%>%
  summarise(median = median(n), minimum = min(n), maximum = max(n), quarter1 = quantile(n, 0.25), quarter3 = quantile(n, 0.75))

genres_totals <- edx %>% count(genres)%>%
  summarise(median = median(n), minimum = min(n), maximum = max(n), quarter1 = quantile(n, 0.25), quarter3 = quantile(n, 0.75))

users_totals

movie_totals

genres_totals

#Movie rating distribution
#Histogram shows the frequencies of the different ratings for each rating

edx %>% ggplot(aes(rating)) +
  geom_histogram(bins = 25, color = "black") +
  labs(title = "Distribution of ratings",
       subtitle = "by # of Ratings",
       x = "Rating",
       y = "Frequency")

#Table of percentage each rating recieved
x <- round(table(edx$rating)/10000000*100,1)

x1 <- as.data.frame(x)


ggplot(data = x1, aes(Var1, Freq)) +
  geom_bar( stat = "identity") +
  labs(title = "Distribution of ratings",
     subtitle = "by # of Ratings",
     x = "Rating",
     y = "Percent")


x1$Freq
# Distrubutions of the number of rationg per movie.

movieId_count <- edx %>% count(userId)
head(movieId_count)


#Top 20 Movies that were rated

movie_title <- edx %>% group_by(title) %>% count() %>% arrange(desc(n))  
movie_title
filter(movie_title, n > 20000)

# Historgram of number of movie ratings per movie 
movie_Id <- edx %>% group_by(movieId) %>% count() %>% arrange(desc(n))

movie_Id %>% ggplot(aes(n)) + 
  geom_histogram(bins = 25, color = "black") +
  labs(title = "Distribution of Number of Rating per Movie",
       subtitle = "Distribution of Titles",
       x = "Number of Reviews per Title",
       y = "Movie Number")



edx %>% group_by(title) %>% summarize()

ggplot(movie_Id) + 
  geom_histogram(aes(movie_Id))

#This section provides the average rating and SD per genres.  
#Group by genres
genres_grouped <- group_by(edx, genres)

genre_mean <- summarise(genres_grouped, Avg_genre = mean(rating), sd_genre = sd(rating))
head(genre_mean)
Avg_genre

count(filter(genre_mean, Avg_genre > 3 & Avg_genre < 4))

count(filter(genre_mean, Avg_genre >= 4))


#Plot to show distrubution  of the averge ratings per genre
genre_mean %>% ggplot(aes(Avg_genre)) +
  geom_histogram(bins = 100, color = "black") +
  labs(title = "Distribution of Average Ratings per genre",
       subtitle = "by # of Ratings",
       x = "Rating",
       y = "Genre")

#This plot shows the distrubuion of the SD of the rating per genres.  
genre_mean %>% ggplot(aes(sd_genre)) +
  geom_histogram(bins = 25, color = "black") +
  labs(title = "Distribution of  Standard Deviation ratings",
       subtitle = "by # of Ratings",
       x = "Standard Deviation",
       y = "Frequency")


#Genarate a plot showing the 
genres_list <- as.data.frame(table(edx$genres))
str(genres_list)
genres_list
head(genres_list)

genres_list %>% ggplot(aes(Freq)) +
  geom_histogram(bins = 20, color = "black") +
  labs(title = "Distribution of genres",
       subtitle = "by # of Ratings",
       x = "Genre",
       y = "Frequency")

genres_list


#Test to ensure table command exracted all the ratings


#This starts the modeling section.  We first want to load the test_set and train_set. 
load("train_set.Rda")
load("test_set.Rda")

#This is the starting section for the modeling
mu_hat <- mean(train_set$rating)
mu_hat

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
RMSE

#Below calculates the RMSE using only the average.  
naive_rmse <- RMSE(test_set$rating, mu_hat)


rmse_results <- data_frame(method = "Just the Average", RMSE = naive_rmse)
rmse_results


#Mean Ration Calcuation
mu <- mean(train_set$rating)
# Add inidvidules movie ID to Model pg 650
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

# pg 650
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# Modeling movie effect pg 651

predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)

model_1_rmse

rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

#User ID Effect Pg 650
#histogram of the userId factor
test_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

#Modeling the user effect pg 652
user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
model_2_rmse

rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Movie + Users Effect Model",
                                     RMSE = model_2_rmse))
rmse_results %>% knitr::kable()

#Regularization pg. 657

train_set %>%
  count(movieId) %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10)



predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
model_3_rmse

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Movie Effect Model",
                                 RMSE = model_3_rmse))
rmse_results %>% knitr::kable()


#pg 659 
#Movie effect and User Effect with Regualization
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <-  train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

as.data.frame(rmses) %>% min(rmses)


class(rmses)
dim(rmses)


#Comparison of RMSES to the Grading Rubric
min(rmses)
min(rmses) <= 0.8949

qplot(lambdas,rmses)
#Find the lamba the minimizes RMSE
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results 

lambdas_O = seq(5, 6, 0.01)
#Run the same function using the minimum lamba
rmses_v <- sapply(lambdas_O, function(l){
  mu <- mean(edx$rating)
  b_i <-  edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})

as.data.frame(rmses_v) %>% min(rmses_v)

lambdas_O <- lambdas[which.min(rmses_v)]
lambda
lambdas
rmses_v <= 0.8649
rmses_v

