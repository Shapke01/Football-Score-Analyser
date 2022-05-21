library(shiny)
library(shinydashboard)
library(httr)
library(DT)
library(jsonlite)
library(dplyr)
library(Dict)
library(ggplot2)
library(plotly)

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
  
  season <- league_standings$season
  
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
  
  return(dic)
}

top_score <- function(league_name){
  x = leagues$get(league_name)$get("scorers") %>%
    ggplot(aes(x=reorder(player$name, numberOfGoals), y=numberOfGoals)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    geom_text(aes(label=numberOfGoals), vjust=-1) +
    coord_flip() +
    xlab("") +
    theme_bw()
  return(x)
}

leagues <- Dict$new(
  SA = league("SA"),
  PL = league("PL"),
  PD = league("PD"),
  BL1 = league("BL1"),
  FL1 = league("FL1"),
  .overwrite = TRUE
)

all_leagues = rbind(leagues$get("SA")$get("scorers"),
                    leagues$get("PL")$get("scorers"),
                    leagues$get("PD")$get("scorers"),
                    leagues$get("BL1")$get("scorers"),
                    leagues$get("FL1")$get("scorers"))


server <- function(input, output){
  output$SA_matches_table <- renderDataTable({
    leagues$get("SA")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$SA_top_scorers <- renderPlot(
    top_score("SA")
  )
  output$SA_progress <- renderValueBox({
    progress <- as.numeric(Sys.Date() - as.Date(leagues$get("SA")$get("season")$startDate, format = "%Y-%m-%d")) / 
      as.numeric((as.Date(leagues$get("SA")$get("season")$endDate, format = "%Y-%m-%d")) - as.Date(leagues$get("SA")$get("season")$startDate, format = "%Y-%m-%d"))
    valueBox(
      paste0(format(round(progress, 3) *100, nsmall = 2), "%"), "Season Progress", icon = icon("fas fa-trophy"),
      color = "purple",
      width = NULL,
    )
  }
    
  )
  output$PL_matches_table <- renderDataTable({
    leagues$get("PL")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$PL_top_scorers <- renderPlot(
    top_score("PL")
  )
  
  output$PD_matches_table <- renderDataTable({
    leagues$get("PD")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$PD_top_scorers <- renderPlot(
    top_score("PD")
  )
  
  output$BL1_matches_table <- renderDataTable({
    leagues$get("BL1")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$BL1_top_scorers <- renderPlot(
    top_score("BL1")
  )
  
  output$FL1_matches_table <- renderDataTable({
    leagues$get("FL1")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$FL1_top_scorers <- renderPlot(
    top_score("FL1")
  )
  
  output$CMP_golden_shoe <- renderValueBox({
    valueBox(all_leagues[which.max(all_leagues$numberOfGoals),]$player$name,
             "European Golden Shoe", 
             icon = icon("fa-solid fa-shoe-prints"),
             color = "yellow")
  })
  
  
}

