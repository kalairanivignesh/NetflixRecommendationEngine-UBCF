#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Loading libraries
library(shiny)
#remotes::install_github("daqana/dqshiny") # to download directly from github
library(dqshiny)
library(recommenderlab)
library(DT)
library(xtable)
library(data.table)
library(magrittr)
memory.limit(100000)

load("../../Data/movies.data")
load("../../Data/ratings.data")

load("../../Data/MovieRating_Triplets.data")
load("../../Data/MovieRating_Merged.data")
load("../../Data/Movie_Recommender_Model.data")
load("../../Data/MovieRatingMatrix.data")
#storing only unique userid's
userIDList <- unique(Ratings$userId)

findUserSummary <- function(id) {
    data.table("Number of Movies Rated"=nrow(MovieRating_Merged[userId==id]),
               "Average Rating"=mean(MovieRating_Merged[userId==id]$rating),
               "Last Rating Date" = as.character(as.POSIXct(max(MovieRating_Merged[userId==id]$timestamp), origin = "1970-01-01")))
}


ui <- fluidPage(
    
  tags$head(
    tags$style(HTML("
      .styled-table {
    border-collapse: collapse;
    margin: 25px 0;
    font-size: 0.9em;
    font-family: sans-serif;
    min-width: 400px;
    box-shadow: 0 0 20px rgba(0, 0, 0, 0.15);
  }


.styled-table thead tr {
    background-color: #009879;
    color: #ffffff;
    text-align: left;
}

.styled-table th,
.styled-table td {
    padding: 12px 15px;
}

.styled-table tbody tr {
    border-bottom: 1px solid #dddddd;
}

.styled-table tbody tr:nth-of-type(even) {
    background-color: #f3f3f3;
}

.styled-table tbody tr:last-of-type {
    border-bottom: 2px solid #009879;
}

.styled-table tbody tr.active-row {
    font-weight: bold;
    color: #009879;
}

  .NoHeader th {display:none;}
    "))
  ),
    headerPanel("Movie Recommendation Engine - UBCF"),
    sidebarLayout(
        sidebarPanel(
    
            fluidRow(
                column(4,
                        autocomplete_input("userId", "Enter User Id: ", as.character(userIDList), max_options = 1000,placeholder = "Please enter userId"))
                ),
            fluidRow(
                column(3, actionButton("recommend", "Recommend Movie")) 
            ), fluidRow(
              column(12,
                     sliderInput("predictCount", "Number of Movies to Recommend:",
                                 min = 1, max = 10, value = 5
                     )
                  )
            ),
            fluidRow(
              column(6,helpText("Summary of Movie Database"))
            ),
            fluidRow(
              column(7,tags$div(
                class="NoHeader",
                tableOutput("MovieDBSummary"))
              )
            )
          ), 
        mainPanel( 
            fluidRow(
            column(12,htmlOutput("header_str")),
            
            ),
            fluidRow(
            column(5,tags$div(
              class="styled-table", tableOutput("recommendedMovies")
              )
            )
            ),
            fluidRow(
              column(12,helpText("User Summary")),
              
            ),
            fluidRow(
              column(5,tags$div(
                class="styled-table",
                tableOutput("userSummary")
                )
                )
            )
        )
    )
)


# Define server logic ----
server <- function(input, output) {
    
    Recommendation <- reactiveValues(headerStr = "Enter user id and click \"Recommend Movie\" to see recommended movies for the User",
                                     recommendedMovieTable=NULL,
                                     userSummary=NULL)
  
    
    observeEvent(input$recommend, {
      withProgress(message="Predicting recommendation ...",
                   recommendationList <- as(predict(object = MovieRating_Recommender_Model,
                                                    newdata = MovieRatingMatrix[as.character(input$userId)], 
                                                    n = input$predictCount),"list"))
        Recommendation$recommendedMovieTable <- merge(
            data.table("movieId"=as.integer(unlist(recommendationList))),
            Movies
        )[,c(6,5,4)]
        setnames(Recommendation$recommendedMovieTable,"title_new","title")
        Recommendation$recommendedMovieTable$title <- paste0("<a href='https://www.imdb.com/search/title/?title=",Recommendation$recommendedMovieTable$title,"' target='_blank' class='btn btn-primary'>",Recommendation$recommendedMovieTable$title,"</a>")
        
        Recommendation$headerStr <- paste0("Recommended Movies for:",input$userId, " [Based on similar user profiles]")
        Recommendation$userSummary <- findUserSummary(input$userId)
        })
        
    output$header_str <- renderText({
        Recommendation$headerStr
        
    })
    
    output$MovieDBSummary <- renderTable(xtable(data.table(
      "items" = c("Movies Count","Number of Movies Rated", "Number of Users","Average Rating"),
      "values"=c(as.character(length(unique(Movies$movieId))),
                 as.character(length(unique(MovieRating_Merged[!is.na(rating)]$movieId))),
                 as.character(length(unique(MovieRating_Merged$userId))),
                 as.character(round(mean(MovieRating_Merged[!is.na(rating)]$rating),2))))))
    
    output$recommendedMovies <- renderTable(
        if(!is.null(Recommendation$recommendedMovieTable)){
        xtable(Recommendation$recommendedMovieTable)
          }, align="c",
        type = "html",
        width=NULL
        , sanitize.text.function = function(x) x)

    output$userSummary <-     renderTable(
      if(!is.null(Recommendation$userSummary)){
      xtable(Recommendation$userSummary)
        },
      
      type = "html",
      align="c",
      width=NULL
      )
    
}

# Run the app ----
shinyApp(ui = ui, server = server)

