library(shiny)
library(shinydashboard)
library(httr)
library(DT)
library(jsonlite)
library(dplyr)

league_standings = GET("http://api.football-data.org/v2/competitions/SA/standings",
         add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f"))

league_scorers = GET("http://api.football-data.org/v2/competitions/SA/scorers",
         add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f"))

league_standings <- content(league_standings, as="text")
league_standings <- fromJSON(league_standings)

league_scorers <- content(league_scorers, as="text")
league_scorers <- fromJSON(league_scorers)

table <- as.data.frame(league_standings$standings$table)
table <- bind_rows(table)

table <- table %>% as_tibble()

table <- table %>% mutate(name = team$name) %>%
  relocate(name, .after = position) %>%
  select(-form, -team)

scorers <- as.data.frame(league_scorers$scorers)
scorers <- bind_rows(scorers)

scorers <- scorers %>% as_tibble()

server <- function(input, output){
  
  output$matches_table <- renderDataTable({
    table
  })
}
