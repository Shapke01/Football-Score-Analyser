library(shiny)
library(shinydashboard)
library(httr)


server <- function(input, output){
  matches = GET("https://api.football-data.org/v2/competitions",
                add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f")
  )
  
  output$some_plot <- renderPlot({
    plot(iris$Sepal.Length, iris[[input$features]])
  })
}
