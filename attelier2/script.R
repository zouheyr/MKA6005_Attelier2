
#  title: "MBA-6005 TRAVAIL INDIVIDUEL 02"
#  author: Zouheyr Ayas
#  date: "Nov 15, 2020"


library(car)
library(shiny)
library(shinydashboard)
data(Salaries)

histo_salaires <- renderPlot({ hist(Salaries$yrs.service, freq = FALSE, 
                             main="Années de service - fDensité",col="lightgreen",
                             xlab = "Années de services"
                             )})

