library(shiny)
library(shinydashboard)
library(httr)

ui <- dashboardPage(
  dashboardHeader(title = "Leagues"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Serie A", tabName = "SA", icon = icon("fa-solid fa-futbol")),
      menuItem("La Liga", tabName = "PD", icon = icon("fa-solid fa-futbol")),
      menuItem("Premier League", tabName = "PL", icon = icon("fa-solid fa-futbol")),
      menuItem("Ligue 1", tabName = "FL1", icon = icon("fa-solid fa-futbol")),
      menuItem("Bundesliga", tabName = "BL1", icon = icon("fa-solid fa-futbol"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("SA",
              fluidPage(
                h2("League Table"),
                dataTableOutput("SA_matches_table")
              )
      ),
      tabItem("PD",
              fluidPage(
                h2("League Table"),
                dataTableOutput("PD_matches_table")
              )
      ),
      tabItem("PL",
              fluidPage(
                h2("League Table"),
                dataTableOutput("PL_matches_table")
              )
      ),
      tabItem("FL1",
              fluidPage(
                h2("League Table"),
                dataTableOutput("FL1_matches_table")
              )
      ),
      tabItem("BL1",
              fluidPage(
                h2("League Table"),
                dataTableOutput("BL1_matches_table")
              )
      )
      
              
      
    )
  )
)
