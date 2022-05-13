library(shiny)
library(shinydashboard)
library(httr)

ui <- dashboardPage(
  dashboardHeader(title = "Leagues"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Premier League", tabName = "PL", icon = icon("Premier League")),
      menuItem("La Liga", tabName = "LA", icon = icon("La Liga"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("PL",
              fluidPage(
                h2("Table of matches"),
                dataTableOutput("matches_table")
              )
      ),
      
      tabItem("LA", 
              fluidPage(
                h1("Siiiiiiiiiii!"),
                box(
                  
                )
              )
              
      )
    )
  )
)
