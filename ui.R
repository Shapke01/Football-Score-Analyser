library(shiny)
library(shinydashboard)
library(httr)

ui <- dashboardPage(
  dashboardHeader(title = "Leagues"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Serie A", tabName = "SA", icon = icon("Series A")),
      menuItem("La Liga", tabName = "LA", icon = icon("La Liga"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("SA",
              fluidPage(
                h2("League Table"),
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
