library(dplyr)
library(data.table)
library(recommenderlab)
library(tidyr)

##build recommender out of more comprehensive dataset
setwd('C:/Users/there/Documents/Portfolio/MyAnimeList/Data')

anime_data <- fread('anime_cleaned.csv')
animelist_data <- fread('animelists_cleaned.csv')

#Filter out anime with score below the threshold
score_threshold <- 7

animelist_data <- animelist_data %>%
  filter(my_score >= score_threshold)

#Filter on users with at least rated 10 anime on their list
animelist_data <- animelist_data %>%
  group_by(username) %>%
  mutate(anime_count = n()) %>%
  filter(anime_count >= 10) %>%
  ungroup()

unq_users <- animelist_data %>% distinct(username) %>% unlist() %>% as.vector()

sample_users <- sample(unq_users, 1000, replace = FALSE)

animelist_data <- animelist_data %>% filter(username %in% sample_users)

# saveRDS(animelist_data, 'animelist_sample.rds')
animelist_data <- readRDS('animelist_sample.rds')

anime_map <- animelist_data %>% 
  left_join(anime_data, by = 'anime_id') %>% 
  select(anime_id, title, title_english, image_url, genre, type, episodes, score, scored_by, aired_string)

# saveRDS(anime_map, 'anime_map.rds')

model_data <- animelist_data %>%
  mutate(n = 1) %>%
  arrange(anime_id) %>%
  select(username, anime_id, n) %>%
  pivot_wider(names_from = 'anime_id', values_from = 'n', values_fill = list(n = 0)) %>%
  select(-username) %>%
  as.matrix() %>%
  as('binaryRatingMatrix')

# Tune model using 10-fold cross validation.
training_schema <- evaluationScheme(model_data, method = 'cross-validation', k = 10, given = 5)

models_to_evaluate <- list(
  `IBCF Cosinus` = list(name = "IBCF", 
                        param = list(method = "cosine")),
  `IBCF Pearson` = list(name = "IBCF", 
                        param = list(method = "pearson")),
  `UBCF Cosinus` = list(name = "UBCF",
                        param = list(method = "cosine")),
  `UBCF Pearson` = list(name = "UBCF",
                        param = list(method = "pearson")),
  `Random` = list(name = "RANDOM", param=NULL)
)
n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = training_schema, 
                         method = models_to_evaluate, 
                         n = n_recommendations)
plot(list_results)

vector_nn <- c(5, 10, 20, 30, 40)
models_to_evaluate <- lapply(vector_nn, function(nn){
  list(name = "IBCF",
       param = list(method = "pearson", k = nn))
})
names(models_to_evaluate) <- paste0("IBCF with ", vector_nn, " Users")
list_results <- evaluate(x = training_schema, 
                         method = models_to_evaluate, 
                         n = n_recommendations)
plot(list_results)

# Final model is IBCF Pearson model based on 10 nearest neighbors
rec_mod <- Recommender(model_data, method = 'IBCF', param = list(method = 'pearson', k = 10))

# saveRDS(rec_mod, file = 'anime_rec_model.rds')

# Function to return recommendations based on user input of 5 anime titels
getRecommendations <- function(userinput){
  userinput_id <- anime_map %>% 
    filter(title %in% userinput) %>% 
    distinct(anime_id) %>% 
    unlist() %>% 
    as.vector()
  
  user_anime <- animelist_data %>%
    filter(anime_id %in% userinput_id) %>%
    distinct(anime_id) %>%
    rbind(animelist_data %>% distinct(anime_id)) %>%
    count(anime_id) %>%
    mutate(n = n - 1) %>%
    pivot_wider(names_from = 'anime_id', values_from = 'n', values_fill = list(n = 0)) %>%
    as.matrix() %>%
    as('binaryRatingMatrix')
  
  anime_rec <- predict(rec_mod, user_anime, n = 10)
  anime_rec_list <- as(anime_rec, 'list') %>% unlist()
  anime_rec_df <- data.frame(anime_id = anime_rec_list)
  
  anime_rec_df <- anime_rec_df %>% 
    mutate(anime_id = as.integer(anime_id)) %>% 
    left_join(anime_map, by = 'anime_id') %>%
    distinct()
  
  return(anime_rec_df)
}

# Test
tato_anime <- c('Devilman: Crybaby', 'Baki', 'FLCL', 'Monthly Girls\' Nozaki-Kun', 'Kaiba')
getRecommendations(tato_anime)
