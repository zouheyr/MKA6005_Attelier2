library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "TRAVAIL INDIVIDUEL 02"),
  dashboardSidebar(
    menuItem("Global"),
    menuSubItem("submenu"),
    menuItem("finances"),
    menuItem("Marketing")
  ), 
  dashboardBody(
    
    box(width = 6,
        title = "Graph 2",
        color = "red", ribbon = TRUE, title_side = "top right",
        column(width = 8,
               plotOutput("histo_salaires")
        )
    )
  ))