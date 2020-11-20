source("script.R")
#source("Travail_individuel_02.R")
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

shinyServer(function(input, output){ 
  
  output$histo_salaires <- histo_salaires
  #output$histo_depenses_magasin <- histo_depenses_magasin
  #output$boxplot_salaires<-boxplot_salaires
  
  })