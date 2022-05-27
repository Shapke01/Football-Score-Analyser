library(shiny)
library(shinydashboard)
library(httr)
library(DT)
library(jsonlite)
library(dplyr)
library(Dict)
library(ggplot2)
library(plotly)

league_fluid_page <- function(league_name){
  x = fluidPage(
    
    column(width=6,
           box(
             title = "League Table",
             solidHeader = TRUE,
             width = NULL,
             height = NULL,
             status = "primary",
             dataTableOutput(paste0(league_name,"_matches_table"), height = "400px")
           ), 
           plotOutput(paste0(league_name,"_scatter_plot")),
           ),
    
    column(width=5,
           box(
             title = "Top scorers",
             solidHeader = TRUE,
             width = NULL,
             height = NULL,
             status = "primary",
             plotOutput(paste0(league_name,"_top_scorers"))
           ),
           valueBoxOutput(paste0(league_name,"_progress")),
           plotlyOutput(paste0(league_name,"_polar_plot"))
    )
  )
  return(x)
}


header <- dashboardHeader(title = tags$img(src = "logo.jpg"))

sideBar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "AB", icon = icon("fa-solid fa-question")),
    menuItem("Leagues", tabName = "LEAGUES", icon = icon("fa-solid fa-align-justify"),
             menuSubItem("Serie A", tabName = "SA", icon = icon("fa-solid fa-futbol")),
             menuSubItem("La Liga", tabName = "PD", icon = icon("fa-solid fa-futbol")),
             menuSubItem("Premier League", tabName = "PL", icon = icon("fa-solid fa-futbol")),
             menuSubItem("Ligue 1", tabName = "FL1", icon = icon("fa-solid fa-futbol")),
             menuSubItem("Bundesliga", tabName = "BL1", icon = icon("fa-solid fa-futbol"))
    ),
    menuItem("Comparator", tabName = "CMP", icon = icon("fa-solid fa-sort"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabItems(
    # About
    tabItem("AB",
            fluidPage(
              h2("Soccer Scores Analyzer")
            )
    ),
    #Series A
    tabItem("SA",
            league_fluid_page("SA")
    ),
    # La Liga
    tabItem("PD",
            league_fluid_page("PD")
    ),
    #Premier League
    tabItem("PL",
            league_fluid_page("PL")
    ),
    #Ligue 1
    tabItem("FL1",
            league_fluid_page("FL1")
    ),
    #Bundesliga
    tabItem("BL1",
            league_fluid_page("BL1")
    ),
    
    tabItem("CMP",
            fluidPage(
              valueBoxOutput("CMP_golden_shoe"),
              dataTableOutput("CMP_scorrers")
            )
    )
  )
)

ui <- dashboardPage(header, sideBar, body)
