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
    select(-form, -team, -position)
  
  season <- league_standings$season
  
  league_scorers = GET(paste("http://api.football-data.org/v2/competitions/",league_name,"/scorers", sep = ""),
                       add_headers("X-Auth-Token"="0fdd6155b06b4707bda7e82a11b5c44f"))
  league_scorers <- content(league_scorers, as="text")
  league_scorers <- fromJSON(league_scorers)
  scorers <- as.data.frame(league_scorers$scorers)
  scorers <- bind_rows(scorers)
  scorers <- scorers %>% as_tibble()
  
  dic <- Dict$new(
    table = table,
    season = season,
    scorers = scorers,
    matches = matches,
    .overwrite = TRUE
  )
  
  return(dic)
}

league_datatable <- function(league_name){
  
  table <- leagues$get(league_name)$get("table") %>% 
    select(-goalsFor, -goalsAgainst)
  datatable(table, options = list(scrollY = "400px", lengthMenu = c(5, 10, 20, 30), pageLength=10))
}

top_score <- function(league_name){
  x = leagues$get(league_name)$get("scorers") %>%
    ggplot(aes(x=reorder(player$name, numberOfGoals), y=numberOfGoals)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    geom_text(aes(label=numberOfGoals), vjust=-1) +
    theme(text = element_text(size = 20)) +
    coord_flip() +
    xlab("")
    #theme_bw()
  return(x)
}

season_progress <- function(league_name){
  progress <- as.numeric(Sys.Date() - as.Date(leagues$get(league_name)$get("season")$startDate, format = "%Y-%m-%d")) / 
    as.numeric((as.Date(leagues$get(league_name)$get("season")$endDate, format = "%Y-%m-%d")) - as.Date(leagues$get(league_name)$get("season")$startDate, format = "%Y-%m-%d"))
  x = valueBox(
    paste0(format(min(round(progress, 3) *100, 100), nsmall = 1), "%"), "Season Progress", icon = icon("fas fa-trophy"),
    color = "purple",
    width = NULL
  )
  return(x)
}

plot_goals <- function(input, league_name){
  table <- leagues$get(league_name)$get("table")
  s = input[[paste(league_name,"_matches_table_rows_selected", sep="")]]
  par(mar = c(4, 4, 1, .1))
  goals <- table %>% select(goalsFor, goalsAgainst)
  plot(goals, cex.axis=1.2, cex.lab=1.5, pch=c("⚽"),
  xlab = "Goals For", ylab = "Gols Against", bty='n')
  
  if (length(s)) points(goals[s, , drop = FALSE], pch = c("🥅"), cex = 3)
  
}

polar_plot <- function(input, league_name){
  s = input[[paste(league_name,"_matches_table_rows_selected", sep="")]]
  
  x = plot_ly(
    type = "scatterpolar",
    mode = "markers",
    r = c(0,0,0),
    theta = c("won", "draw", "lost")
  )
  if (length(s) == 1) {
    win_or_loose = leagues$get(league_name)$get("table") %>% select(won, draw, lost) %>% slice(s)
    values = unlist(c(as.numeric(win_or_loose%>%select(won)),
                      as.numeric(win_or_loose%>%select(draw)),
                      as.numeric(win_or_loose%>%select(lost))))
    x = plot_ly(
      type = "scatterpolar",
      mode = "markers",
      r = values,
      theta = c("won", "draw", "lost"),
      fill = "toself"
    ) %>% 
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,max(values))
          )
        ),
        showlegend = F
      )
  }
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

all_leagues = bind_rows((leagues$get("SA")$get("scorers")) ,
                    (leagues$get("PL")$get("scorers")) ,
                    (leagues$get("PD")$get("scorers")) ,
                    (leagues$get("BL1")$get("scorers")) ,
                    (leagues$get("FL1")$get("scorers")))

all_league_scorers <- function(){
  table <- mutate(all_leagues, Name = player$name, Nationality = player$nationality, Team = team$name, Goals = numberOfGoals) %>%
  select(Name, Nationality, Team, Goals) %>%
  arrange(desc(Goals))
  return(table)
  }

server <- function(input, output){
  
  #### SERIES A
  output$SA_matches_table <- renderDataTable({
    league_datatable("SA")
  })
  output$SA_top_scorers <- renderPlot(
    top_score("SA")
  )
  output$SA_progress <- renderValueBox({
    season_progress("SA")
  })
  output$SA_scatter_plot <- renderPlot(
    plot_goals(input, "SA")
  )
  output$SA_polar_plot = renderPlotly({
    polar_plot(input, "SA")
  })
    
  #### PREMIER LEAGUE
  output$PL_matches_table <- renderDataTable({
    leagues$get("PL")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$PL_top_scorers <- renderPlot(
    top_score("PL")
  )
  output$PL_progress <- renderValueBox({
    season_progress("PL")
  })
  output$PL_scatter_plot <- renderPlot(
    plot_goals(input, "PL")
  )
  output$PL_polar_plot = renderPlotly({
    polar_plot(input, "PL")
  })
  
  #### LA LIGA
  output$PD_matches_table <- renderDataTable({
    leagues$get("PD")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$PD_top_scorers <- renderPlot(
    top_score("PD")
  )
  output$PD_progress <- renderValueBox({
    season_progress("PD")
  })
  output$PD_scatter_plot <- renderPlot(
    plot_goals(input, "PD")
  )
  output$PD_polar_plot = renderPlotly({
    polar_plot(input, "PD")
  })
  
  #### BUNDESLIGA
  output$BL1_matches_table <- renderDataTable({
    leagues$get("BL1")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$BL1_top_scorers <- renderPlot(
    top_score("BL1")
  )
  output$BL1_progress <- renderValueBox({
    season_progress("BL1")
  })
  output$BL1_scatter_plot <- renderPlot(
    plot_goals(input, "BL1")
  )
  output$BL1_polar_plot = renderPlotly({
    polar_plot(input, "BL1")
  })
  
  #### LIGUE 1
  output$FL1_matches_table <- renderDataTable({
    leagues$get("FL1")$get("table") %>% 
      select(-goalsFor, -goalsAgainst)
  })
  output$FL1_top_scorers <- renderPlot(
    top_score("FL1")
  )
  output$FL1_progress <- renderValueBox({
    season_progress("FL1")
  })
  output$FL1_scatter_plot <- renderPlot(
    plot_goals(input, "FL1")
  )
  output$FL1_polar_plot = renderPlotly({
    polar_plot(input, "FL1")
  })
  
  #### COMPARE
  output$CMP_golden_shoe <- renderValueBox({
    valueBox(all_leagues[which.max(all_leagues$numberOfGoals),]$player$name,
             "European Golden Shoe", 
             icon = icon("fa-solid fa-shoe-prints"),
             color = "yellow")
  })
  output$CMP_scorrers <- renderDataTable({
    datatable(all_league_scorers(), options = list(scrollY = "400px", lengthMenu = c(5, 10, 20, 30), pageLength=10))
  })
}

