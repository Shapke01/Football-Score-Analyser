library(shiny)
library(shinydashboard)
library(httr)

ui <- dashboardPage(
  dashboardHeader(title = "Leagues"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Serie A", tabName = "SA", icon = icon("fa-solid fa-futbol")),
      menuItem("La Liga", tabName = "LA", icon = icon("fa-solid fa-futbol"))
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
