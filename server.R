library(shiny)
library(shinydashboard)
library(httr)
library(DT)
library(jsonlite)

SA = GET("http://api.football-data.org/v2/competitions/SA/standings",
         add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f"))

SA <- content(SA, as="text")
SA <- fromJSON(SA)

table <- as.data.frame(SA$standings$table)
table <- bind_rows(table)

table <- table %>% as_tibble()


table <- table %>% mutate(name = team$name) %>%
  relocate(name, .after = position) %>%
  select(-form, -team)

table

server <- function(input, output){
  
  
  output$matches_table <- renderDataTable({
    table
  })
}
