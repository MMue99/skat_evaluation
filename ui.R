library(openxlsx)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DBI)
library(reshape)
library(shinyWidgets)
library(rsconnect)
library(shinydashboard)
library(shinyjs)


jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

shinyUI(
  
  fluidPage(
    #jshiny-------
    
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("disableTab","enableTab")),
    inlineCSS(css),
    #Layout
    theme = shinythemes::shinytheme('darkly'),
    titlePanel("Uljamins Skatrunde"),
    
    #Sidebar Layout
    #sidebarLayout(
    #Sidebar for Inputs
    #Main panel for plots
    #mainPanel(
    tabsetPanel(
      ##Datei Upload-----
      tabPanel("Datei_Upload",
               sidebarLayout(
                 sidebarPanel(
                   fileInput('Upload', 'Upload einer xlsx Datei',
                             accept = c(".xlsx"))
                 ),
                 mainPanel(tableOutput("contents"))
               ),
      ),
      ##Verlauf------
      tabPanel("Verlaufsgrafik",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("Season_input"),
                   uiOutput("Spieler_input")
                 ),
                 mainPanel(
                   p(plotOutput("Verlauf_plot")),
                   #br(),
                   p("Punkteverlauf der ausgewaehlten Season")
                 )
               )
      ),
      ##Auswertungen-------
      tabPanel("Auswertungen",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("Kennzahl_Kennzahl"),
                   uiOutput("Spieler_Kennzahl")
                 ),
                 mainPanel(
                   p(plotOutput("Kennzahl_plot"))
                 )
               )
      ),
      ##Faktoren--------
      tabPanel("Faktoren",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("Season_Faktor"),
                   uiOutput("Spieler_Faktor"),
                   uiOutput("Gewonnen_Faktor")
                 ),
                 mainPanel(
                   p(plotOutput("Faktor_plot_individual")),
                   #br(),
                   p("Histogramm fuer Spielfaktor, aufgeschluesselt nach Spielern")
                 )
               )
               
      ),
      ##Spiele------
      tabPanel("Spiele",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("Season_Spiele"),
                   uiOutput("Spieler_Spiele"),
                   uiOutput("Spiel_Spiele")
                 ),
                 mainPanel(
                   p(plotOutput("Spielwert_plot")),
                   #br(),
                   p("Histogram fuer Spielwerte, aufgeschluesselt nach Anzahl der Buben waehrend des Spiels"),
                   br(),
                   plotOutput("Spielwert_plot2"),
                   #br(),
                   p("Histogram fuer Spielwerte, aufgeschluesselt nach Spieler")
                 )
               )
      )
      
    ) #End of tabsetPanel
    #)#End of Mainpanel
    #)#End of Sidebar Layout
  )#End of FluidPage
  
)