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
                box(plotOutput("some_plot"), width = 8),
                box(
                  selectInput("features", "Features:",
                              c("Sepal.Width", "Petal.Length", "Petal.Width")), width = 4
                )
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
