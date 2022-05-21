library(shiny)
library(shinydashboard)
library(httr)

header <- dashboardHeader(title = "Football Score Analyzer")

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
  tabItems(
    # About
    tabItem("AB",
            fluidPage(
              h2("Soccer Scores Analyzer")
            )
    ),
    #Series A
    tabItem("SA",
            fluidPage(
              
              column(width=6,
              box(
                title = "League Table",
                solidHeader = TRUE,
                width = NULL,
                status = "primary",
                dataTableOutput("SA_matches_table")
              ), 
              box(
                title = "Top scorers",
                solidHeader = TRUE,
                width = NULL,
                height = 300,
                status = "primary",
                plotOutput("SA_top_scorers")
              )
              ),
              
              column(width=4,
                      valueBoxOutput("SA_progress")
            )
            )
    ),
    # La Liga
    tabItem("PD",
            fluidPage(
              box(
                title = "League Table",
                dataTableOutput("PD_matches_table")
              ),
              box(
                title = "Top scorers",
                plotOutput("PD_top_scorers")
              )
            )
    ),
    #Premier League
    tabItem("PL",
            fluidPage(
              box(
                title = "League Table",
                dataTableOutput("PL_matches_table")
              ),
              box(
                title = "Top scorers",
                plotOutput("PL_top_scorers")
              )
              
            )
    ),
    #Ligue 1
    tabItem("FL1",
            fluidPage(
              box(
                title = "League Table",
                dataTableOutput("FL1_matches_table")
              ),
              box(
                title = "Top scorers",
                plotOutput("FL1_top_scorers")
              )
            )
    ),
    #Bundesliga
    tabItem("BL1",
            fluidPage(
              box(
                title = "League Table",
                dataTableOutput("BL1_matches_table")
              ),
              box(
                title = "Top scorers",
                plotOutput("BL1_top_scorers")
              )
              
            )
    ),
    
    tabItem("CMP",
            fluidPage(
              h2("AAAAAAAAAA")
            )
    )
  )
)

ui <- dashboardPage(header, sideBar, body)
