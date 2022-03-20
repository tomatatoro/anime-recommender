#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list = ls())

library(shiny)
library(dplyr)
library(data.table)
library(recommenderlab)
library(tidyr)
library(rvest)
library(stringr)
library(shinybusy)

anime_map <- readRDS('data/anime_map.rds')
animelist_data <- readRDS('data/animelist_sample.rds')
rec_mod <- readRDS('data/anime_rec_model.rds')

anime_map <- anime_map %>% 
  mutate(title_clean = ifelse(title_english == '', title, title_english),
         image_url = gsub('https://myanimelist.cdn-dena.com/', 'https://cdn.myanimelist.net/', image_url))

unq_anime <- anime_map %>% 
  distinct(title_clean) %>% 
  arrange(title_clean)

getRecommendations <- function(userinput){
  userinput_id <- anime_map %>% 
    filter(title_clean %in% userinput) %>% 
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


renderRec <- function(myresults, i){
  myurl <- paste0('https://myanimelist.net/anime/', myresults$anime_id[i])

  tryCatch({
    # myhtml <- read_html(myurl)
    # 
    # anime_image <- myhtml %>% html_elements('img')
    # anime_image <- anime_image[grepl('itemprop', anime_image)]
    # 
    # anime_image_url <- str_extract(anime_image %>% as.character(), 'https://.*.jpg')

    renderText({paste(
      paste0('<h4 align = "center"><a href = ', myurl, '>',myresults$title_clean[i], ' (', myresults$type[i],')','</a></h4>'),
      paste0('<img src="', myresults$image_url[i],'"width = "90%" height = "90%" align = "center">'),
      paste('<b>Episodes:</b>', myresults$episodes[i]),
      paste('<b>Genre:</b>', myresults$genre[i]),
      paste('<b>Aired:</b>', myresults$aired_string[i]),
      paste('<b>Score:</b>', myresults$score[i]),
      paste('<b>Scored by:</b>', format(myresults$scored_by[i], big.mark = ','), 'users</p><br>'),
      sep = '<br>')})
    
  }, error = function(e){
    renderText('Anime Link Not Available')
  })
}

selectAnimeInput <- function(i){
  selectizeInput(paste0('choice', i), paste0('Anime ', i, ':'), choices = unq_anime, options = list(
    placeholder = 'Please enter anime title.',
    onInitialize = I('function() { this.setValue(""); }')
  ))
} 

ui <- fluidPage(
    # Application title
    titlePanel("Tato's Anime Recommender"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText('Enter your top 5 anime...'),
            
            selectAnimeInput(1),
            selectAnimeInput(2),
            selectAnimeInput(3),
            selectAnimeInput(4),
            selectAnimeInput(5),
            
            actionButton('run', 'Run'),
            
            em(h5('Data Source: Matej Racinsky, "MyAnimeList Dataset." Kaggle, June 2018.')),
            
            width = 3
        ),

        mainPanel(fluidRow(column(width = 2, htmlOutput('rec_01')),
                           column(width = 2, htmlOutput('rec_02')),
                           column(width = 2, htmlOutput('rec_03')),
                           column(width = 2, htmlOutput('rec_04')),
                           column(width = 2, htmlOutput('rec_05'))),
           fluidRow(column(width = 2, htmlOutput('rec_06')),
                    column(width = 2, htmlOutput('rec_07')),
                    column(width = 2, htmlOutput('rec_08')),
                    column(width = 2, htmlOutput('rec_09')),
                    column(width = 2, htmlOutput('rec_10')))
        )
    ), 
    add_busy_spinner(spin = "fading-circle", position = 'full-page')
)

server <- function(input, output, session) {
  
  # updateSelectizeInput(session, 'choice1', choices = unq_anime, server = TRUE)
  
    observeEvent(input$run, {
        user_anime <- c(input$choice1,
                        input$choice2,
                        input$choice3,
                        input$choice4,
                        input$choice5)

        user_results <- getRecommendations(user_anime)
        
        output$rec_01 <- renderRec(user_results, 1)
        output$rec_02 <- renderRec(user_results, 2)
        output$rec_03 <- renderRec(user_results, 3)
        output$rec_04 <- renderRec(user_results, 4)
        output$rec_05 <- renderRec(user_results, 5)
        output$rec_06 <- renderRec(user_results, 6)
        output$rec_07 <- renderRec(user_results, 7)
        output$rec_08 <- renderRec(user_results, 8)
        output$rec_09 <- renderRec(user_results, 9)
        output$rec_10 <- renderRec(user_results, 10)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
