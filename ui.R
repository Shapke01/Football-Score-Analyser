library(shiny)
library(shinydashboard)
library(httr)
library(DT)
library(jsonlite)
library(dplyr)
library(Dict)
library(ggplot2)
library(plotly)
library(shinyWidgets)

league_fluid_page <- function(league_name){
  x = fluidPage(
    
    column(width=7,
           box(
             title = "League Table",
             solidHeader = TRUE,
             height = NULL,
             status = "primary",
             width = 12,
             dataTableOutput(paste0(league_name,"_matches_table"), height = "400px")
           ), 
           box(
             solidHeader = TRUE,
             height = NULL,
             status = "primary",
             width = 12,
             title = "Goals scored and lost",
             plotOutput(paste0(league_name,"_scatter_plot")),
           )
          ),
    
    column(width=5,
           box(
             
             title = "Top scorers",
             solidHeader = TRUE,
             width = 12,
             height = NULL,
             status = "primary",
             plotOutput(paste0(league_name,"_top_scorers"), height = "280px"),
             sliderInput(paste0(league_name,"_range"), "Range of goals:",
                         min=0, max=50, value=c(0,50), step=1)
           ),
           valueBoxOutput(paste0(league_name,"_progress"), width = 8),
           valueBoxOutput(paste0(league_name,"_match_day"), width = 4),
           box(
             title = "Win/draw/lose",
             solidHeader = TRUE,
             width = 12,
             height = NULL,
             status = "primary",
             plotlyOutput(paste0(league_name,"_polar_plot"))
           )
           
    )
  )
  return(x)
}


header <- dashboardHeader(title = tags$img(src = "logo.png"))

sideBar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "AB", icon = icon("fa-solid fa-question", verify_fa = FALSE)),
    menuItem("Leagues", tabName = "LEAGUES", icon = icon("fa-solid fa-align-justify", verify_fa = FALSE),
             menuSubItem("Serie A", tabName = "SA", icon = icon("fa-solid fa-futbol", verify_fa = FALSE)),
             menuSubItem("La Liga", tabName = "PD", icon = icon("fa-solid fa-futbol", verify_fa = FALSE)),
             menuSubItem("Premier League", tabName = "PL", icon = icon("fa-solid fa-futbol", verify_fa = FALSE)),
             menuSubItem("Ligue 1", tabName = "FL1", icon = icon("fa-solid fa-futbol", verify_fa = FALSE)),
             menuSubItem("Bundesliga", tabName = "BL1", icon = icon("fa-solid fa-futbol", verify_fa = FALSE))
    ),
    menuItem("Comparator", tabName = "CMP", icon = icon("fa-solid fa-sort", verify_fa = FALSE))
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
              valueBoxOutput("CMP_golden_shoe", width = 12),
              box(
                title = "Golden Shoe Table",
                solidHeader = TRUE,
                height = NULL,
                status = "primary",
                dataTableOutput("CMP_scorrers")
              ),
              box(
                title = "Most Interesting Leagues",
                solidHeader = TRUE,
                height = NULL,
                status = "primary",
                plotOutput("CMP_sum"),
                  switchInput(inputId = "switch",
                              label = "Change",
                              offLabel = "Goals of TOP 10 Players",
                              onLabel = "All goals")
              )
            )
    )
  )
)

ui <- dashboardPage(header, sideBar, body)
