library(shiny)
library(shinydashboard)
library(httr)
library(DT)
library(jsonlite)
library(dplyr)

league <- function(league_name){
  
  league_standings = GET(paste("http://api.football-data.org/v2/competitions/",league_name,"/standings", sep = ""),
                         add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f"))
  league_standings <- content(league_standings, as="text")
  league_standings <- fromJSON(league_standings)
  table <- as.data.frame(league_standings$standings$table)
  table <- bind_rows(table)
  table <- table %>% as_tibble()
  table <- table %>% mutate(name = team$name) %>%
    relocate(name, .after = position) %>%
    select(-form, -team)
  season <- as.data.frame(league_standings$standings$season)
  season <- bind_rows(season)
  season <- season %>% as_tibble()
  
  league_scorers = GET(paste("http://api.football-data.org/v2/competitions/",league_name,"/scorers", sep = ""),
                       add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f"))
  league_scorers <- content(league_scorers, as="text")
  league_scorers <- fromJSON(league_scorers)
  scorers <- as.data.frame(league_scorers$scorers)
  scorers <- bind_rows(scorers)
  scorers <- scorers %>% as_tibble()
  
#  league_matches = GET(paste("http://api.football-data.org/v2/competitions/",league_name,"/matches", sep = ""),
#                       add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f"))
#  league_matches <- content(league_matches, as="text")
#  league_matches <- fromJSON(league_matches)
#  matches <- as.data.frame(league_matches$matches)
#  matches <- bind_rows(matches)
#  matches <- matches %>% as_tibble()
  
  
  dic <- Dict$new(
    table = table,
    season = season,
    scorers = scorers,
    matches = matches,
    .overwrite = TRUE
  )
  
  print(dic$get("table"))
  
  return(dic)
}

leagues <- Dict$new(
  SA = league("SA"),
  PL = league("PL"),
  PD = league("PD"),
  BL1 = league("BL1"),
  FL1 = league("FL1"),
  .overwrite = TRUE
)

a = leagues$get("SA")$get("table")

server <- function(input, output){
  
  output$matches_table <- renderDataTable({
    leagues$get("BL1")$get("table")
  })
}
