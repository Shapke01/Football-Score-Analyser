library(shiny)
library(shinydashboard)
library(httr)

ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "AB", icon = icon("fa-solid fa-question")),
      menuItem("Leagues", tabName = "LEAGUES", icon = icon("fa-solid fa-align-justify"),
               menuSubItem("Serie A", tabName = "SA", icon = icon("fa-solid fa-futbol")),
               menuSubItem("La Liga", tabName = "PD", icon = icon("fa-solid fa-futbol")),
               menuSubItem("Premier League", tabName = "PL", icon = icon("fa-solid fa-futbol")),
               menuSubItem("Ligue 1", tabName = "FL1", icon = icon("fa-solid fa-futbol")),
               menuSubItem("Bundesliga", tabName = "BL1", icon = icon("fa-solid fa-futbol"))
               ),
      menuItem("Comparator", tabName = "CMP", icon = icon("fa-solid fa-arrows-left-right"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # About
      tabItem("AB",
              fluidPage(
                h2("Soccer Scores Analyzer"),
              )
      ),
      #Series A
      tabItem("SA",
              fluidPage(
                h2("League Table"),
                dataTableOutput("SA_matches_table")
              )
      ),
      # La Liga
      tabItem("PD",
              fluidPage(
                h2("League Table"),
                dataTableOutput("PD_matches_table")
              )
      ),
      #Premier League
      tabItem("PL",
              fluidPage(
                h2("League Table"),
                dataTableOutput("PL_matches_table")
              )
      ),
      #Ligue 1
      tabItem("FL1",
              fluidPage(
                h2("League Table"),
                dataTableOutput("FL1_matches_table")
              )
      ),
      #Bundesliga
      tabItem("BL1",
              fluidPage(
                h2("League Table"),
                dataTableOutput("BL1_matches_table")
              )
      ),
      
      tabItem("CMP",
              fluidPage(
                h2("AAAAAAAAAA")
              )
      )
    )
  )
)
