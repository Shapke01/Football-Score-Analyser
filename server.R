library(shiny)
library(shinydashboard)
library(httr)
library(DT)
library(jsonlite)


server <- function(input, output){
  matches = GET("https://api.football-data.org/v2/competitions",
                add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f")
  )
  
  matches <- content(matches, as="text")
  matches <- fromJSON(matches)
  
  output$matches_table <- renderDataTable({
    matches$competitions
  })
}
