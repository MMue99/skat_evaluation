#Skat Shiy App
#Preliminary Stuff-------
setwd("C:/Users/Nutzer/Desktop/Skat/skat_evaluation")

#install.packages("shinythemes")
#install.packages("data.table")
#install.packages("rsconnect")
#install.packages("shinydashboard")
#install.packages("shinyjs")


library(openxlsx)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DBI)
library(reshape)
library(shinyWidgets)
library(grf)
library(rsconnect)
library(shinydashboard)
library(shinyjs)

Skat <- read.xlsx("Skat.xlsx")


Skat[is.na(Skat)] <- 0

colnames(Skat)[11:13] <- c("Jan", "Marv", "Till")
Skat$Gewonnen <- ifelse(Skat$`Faktor.Sieg/Niederlage` > 0, 1, 0)
Skat$Faktor.Spiel[Skat$Faktor.Spiel == 0] <- 1

Skat$Anzahl.Buben <- as.factor(Skat$Anzahl.Buben)

#Data Transformation------
##Calculate Results--------
Ergebnisse <- data.frame(0,0,0)
colnames(Ergebnisse) <- c("Jan", "Marv", "Till")

for (i in sort(unique(Skat$Season))) {
  for (j in sort(unique(Skat$Spieler))) {
    Ergebnisse[i,j] <- round(sum(Skat$Punkte[Skat$Season == i & Skat$Spieler == j]))
  }
}


##Experiments with plotting------
Spie_gew <- c("Jan", "Marv")

data_working <- Skat %>%
  filter(Season == 1) %>%
  melt(id.vars = "Anzahl", measure.vars = c("Jan", "Marv", "Till"))

ggplot(data = data_working[data_working$variable %in% Spie_gew,], aes(x = Anzahl, y = value, col = variable)) +
  geom_path(size = 2) +
  labs(y = "Punktzahl", colour = "Spieler")


winrate_working <- Win_rate["Geholte_Punkte"]

ggplot(data = Win_rate, aes(x = Season, y = Geholte_Punkte, col = Spieler)) +
  geom_line(size = 1) +
  geom_point(size = 2)

test <- Skat %>%
  filter(Season == 1) %>%
  filter(Spieler == 2 | Spieler == 3)

ggplot(data = Skat, aes(x = Spiel.Buben, fill = Anzahl.Buben)) +
  geom_histogram(bins = 16, binwidth = 1) +
  scale_x_continuous(breaks = sort(unique(Skat$Spiel.Buben)))+labs(x = "Spiel", y = "Anzahl", fill = "Anzahl Buben")
  
test <- Win_rate %>%
  group_by(Spieler) %>%
  summarise(Gesamt_Anzahl = sum(Anzahl_Spiele))

##Other evaluations-----

Win_rate <- Skat %>%
  group_by(Season,Spieler) %>%
  summarise(Geholte_Punkte = round(sum(Punkte)),
            Gewonnene_Spiele = sum(Gewonnen),
            Verlorene_Spiele = sum(Gewonnen != 1),
            Anzahl_Spiele = Gewonnene_Spiele + Verlorene_Spiele,
            Maximale_Punkte = round(max(Punkte),2),
            Minimale_Punkte = round(min(Punkte),2),
            Punkte_pro_Spiel = round(sum(Punkte)/Anzahl_Spiele,2),
            Punkte_pro_Sieg = round(sum(Punkte[which(Gewonnen > 0)])/sum(Gewonnen),2),
            Punkte_pro_Niederlage = round(sum(Punkte[which(Gewonnen == 0)])/sum(Gewonnen != 1),2),
            Siegquote = round(Gewonnene_Spiele/Anzahl_Spiele, 2)) %>%
  as_tibble()
Win_rate$Spieler <- ifelse(Win_rate$Spieler == 1, "Jan", ifelse(Win_rate$Spieler == 2, "Marv", "Till"))

Win_rate$Season <- as.numeric(Win_rate$Season)
Win_rate$Spieler <- as.factor(Win_rate$Spieler)
Win_rate[is.na(Win_rate)] <- 0

##Faktor- und Spieldaten


  

#Multiple Pages on Shiny-----

ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme('darkly'),
  titlePanel("Uljamins Skatrunde"),
  tabsetPanel(
    tabPanel("Datei Upload",
             fileInput('Upload', 'Upload einer xlsx Datei',
                       accept = c(".xlsx"))),
    tabPanel("Verlaufsgrafik",
      pickerInput("sel_Season",
                  "Waehle eine Season",
                  multiple = F,
                  selected = "1",
                  choices = sort(unique(Skat$Season)),
                  options = list(`actions-box` = TRUE)),
      pickerInput("sel_Spieler",
                  "Waehle einen/mehrere Spieler",
                  multiple = T,
                  selected = c("Jan","Marv","Till"),
                  choices = c("Jan", "Marv", "Till"),
                  options = list(`actions-box` = TRUE)),
    mainPanel(p(
      plotOutput("Verlauf_plot"),
      br(),
      p("Der gloriose Verlaufs Graph, jetzt fuer alle Spieler und alle Seasons erhaeltlich in Ihrer lokalen Filiale!")
    ))
    ),
    tabPanel("Auswertungen",
             pickerInput("sel_Kennzahl",
                        "Waehle eine Kennzahl",
                        selected = "Geholte_Punkte",
                        choices = c("Geholte_Punkte", "Gewonnene_Spiele", "Verlorene_Spiele", "Anzahl_Spiele", "Maximale_Punkte", "Minimale_Punkte", "Punkte_pro_Spiel", "Punkte_pro_Sieg", "Punkte_pro_Niederlage", "Siegquote"),
                        options = list(`actions-box` = TRUE)),
             pickerInput("sel_Spieler_Kennzahl",
                        "Waehle einen/mehrere Spieler",
                        multiple = T,
                        selected = c("Jan","Marv","Till"),
                        choices = c("Jan", "Marv", "Till"),
                        options = list(`actions-box` = TRUE)),
            mainPanel(p(
              plotOutput("Kennzahl_plot"),
              br(),
              p("Die praezisesten Auswertungen des Sued-Sudans, zertifiziert vom großen Uljamin persoenlich!")
            ))
  ),
  tabPanel("Faktoren",
           pickerInput("sel_Season_Faktor",
                       "Waehle eine Season",
                       multiple = T,
                       selected = "1",
                       choices = sort(unique(Skat$Season)),
                       options = list(`actions-box` = TRUE)),
           pickerInput("sel_Spieler_Faktor",
                       "Waehle einen/mehrere Spieler",
                       multiple = T,
                       selected = c("Jan", "Marv", "Till"),
                       choices = c("Jan", "Marv", "Till"),
                       options = list(`actions-box` = TRUE)),
           pickerInput("sel_Gewonnen_Faktor",
                       "Waehle zwischen gewonnenen/verlorenen Spielen (1=G, 0=V)",
                       multiple = T,
                       selected = c("1","0"),
                       choices = c("1","0"),
                       options = list(`actions-box` = TRUE)),
           mainPanel(p(
             p("Histogramm fuer Spielfaktor, aufgeschluesselt nach Spielern"),
             br(),
             plotOutput("Faktor_plot_individual")
           ))
  ),
  tabPanel("Spiele",
           pickerInput("sel_Season_Spiele",
                       "Waehle eine Season",
                       multiple = T,
                       selected = "1",
                       choices = sort(unique(Skat$Season)),
                       options = list(`actions-box` = TRUE)),
           pickerInput("sel_Spieler_Spiele",
                       "Waehle einen/mehrere Spieler",
                       multiple = T,
                       selected = c("Jan", "Marv", "Till"),
                       choices = c("Jan", "Marv", "Till"),
                       options = list(`actions-box` = TRUE)),
           pickerInput("sel_Spiel_Spiele",
                       "Waehle ein/mehrere Spiele",
                       multiple = T,
                       selected = c("-4","-3","-2","-1","0","1","2","3","4"),
                       choices = c("-4","-3","-2","-1","0","1","2","3","4"),
                       options = list(`actions-box` = TRUE)),
           mainPanel(p(
             p("Histogram fuer Spielwerte, aufgeschluesselt nach Anzahl der Buben waehrend des Spiels"),
             br(),
             plotOutput("Spielwert_plot"),
             br(),
             p("Histogram fuer Spielwerte, aufgeschluesselt nach Spieler"),
             plotOutput("Spielwert_plot2")
           ))
           )
)
)

server <- function(
  input,
  output,
  session
){
  library(ggplot2)
  library(dplyr)
  library(reshape)
  library(openxlsx)
  
  Skat
  
  #Skat <- reactive(input$Upload$datapath
  #  )
  #
  #if(is.null(Skat)) {
  #  return(NULL)
  #} else{  
    
    Skat[is.na(Skat)] <- 0
    
    colnames(Skat)[11:13] <- c("Jan", "Marv", "Till")
    Skat$Gewonnen <- ifelse(Skat$`Faktor.Sieg/Niederlage` > 0, 1, 0)
    Skat$Faktor.Spiel[Skat$Faktor.Spiel == 0] <- 1
    
    Skat$Anzahl.Buben <- as.factor(Skat$Anzahl.Buben)
    
    
    Win_rate <- Skat %>%
      group_by(Season,Spieler) %>%
      summarise(Geholte_Punkte = round(sum(Punkte)),
                Gewonnene_Spiele = sum(Gewonnen),
                Verlorene_Spiele = sum(Gewonnen != 1),
                Anzahl_Spiele = Gewonnene_Spiele + Verlorene_Spiele,
                Maximale_Punkte = round(max(Punkte),2),
                Minimale_Punkte = round(min(Punkte),2),
                Punkte_pro_Spiel = round(sum(Punkte)/Anzahl_Spiele,2),
                Punkte_pro_Sieg = round(sum(Punkte[which(Gewonnen > 0)])/sum(Gewonnen),2),
                Punkte_pro_Niederlage = round(sum(Punkte[which(Gewonnen == 0)])/sum(Gewonnen != 1),2),
                Siegquote = round(Gewonnene_Spiele/Anzahl_Spiele, 2)) %>%
      as_tibble()
    Win_rate$Spieler <- ifelse(Win_rate$Spieler == 1, "Jan", ifelse(Win_rate$Spieler == 2, "Marv", "Till"))
    
    Win_rate$Season <- as.numeric(Win_rate$Season)
    Win_rate$Spieler <- as.factor(Win_rate$Spieler)
    Win_rate[is.na(Win_rate)] <- 0
    
    
    
    filtered_data <- reactive(Skat %>% 
                                filter(Season == input$sel_Season) %>%
                                melt(id.vars = "Anzahl", measure.vars = c("Jan", "Marv", "Till")) %>%
                                filter(variable %in% input$sel_Spieler))
    output$Verlauf_plot <- renderPlot(
      ggplot(data = filtered_data(), 
             aes_string(x = filtered_data()$Anzahl, y = filtered_data()$value, col = filtered_data()$variable))+
        geom_point(size = 2)+
        geom_path(size = 1) +
        labs(x = "Anzahl", y = "Punktzahl", colour = "Spieler") +
        xlim(c(0,max(filtered_data()$Anzahl + 1))) + 
        ylim(c(min(filtered_data()$value -10),max(filtered_data()$value) + 10))
    )
    output$exp_text <- renderText(paste("some", "text"))
    
    
    Win_rate_filtered <- reactive(Win_rate %>%
                                    filter(Spieler %in% input$sel_Spieler_Kennzahl))
    output$Kennzahl_plot <- renderPlot(
      ggplot(data = Win_rate_filtered(), aes_string(x = Win_rate_filtered()$Season, y = input$sel_Kennzahl, col = Win_rate_filtered()$Spieler)) +
        geom_line(size = 1) + 
        geom_point(size = 2) +
        labs(x = "Season", colour = "Spieler") +
        scale_x_continuous(breaks = sort(unique(Win_rate_filtered()$Season)))
    )
    
    
    Skat_player <- Skat
    Skat_player$Spieler <- ifelse(Skat_player$Spieler == 1, "Jan", ifelse(Skat_player$Spieler == 2, "Marv", "Till"))
    Skat_player$Spieler <- as.factor(Skat_player$Spieler)
    filtered_faktor_data <- reactive(Skat_player %>%
                                       filter(Spieler %in% input$sel_Spieler_Faktor) %>%
                                       filter(Season %in% input$sel_Season_Faktor) %>%
                                       filter(Gewonnen %in% input$sel_Gewonnen_Faktor))
    
    
    output$Faktor_plot_individual <- renderPlot(
      ggplot(data = filtered_faktor_data(), aes_string(x = filtered_faktor_data()$Faktor.Spiel, fill = filtered_faktor_data()$Spieler)) +
        geom_histogram(bins = 16, binwidth = 0.038, origin = -0.019) +
        labs(x = "Faktor", y = "Anzahl", fill = "Spieler")
    )
    
    Spiele_filtered <- reactive(Skat_player %>%
                                  filter(Spieler %in% input$sel_Spieler_Spiele) %>%
                                  filter(Season %in% input$sel_Season_Spiele) %>%
                                  filter(Spiel.Buben %in% input$sel_Spiel_Spiele)) 
    
    
    output$Spielwert_plot <- renderPlot(
      ggplot(data = Spiele_filtered(), aes_string(x = Spiele_filtered()$Spiel.Buben, fill = Spiele_filtered()$Anzahl.Buben)) +
        geom_histogram(bins = 16, binwidth = 1) +
        scale_x_continuous(breaks = seq(from = -4, to = 4, by = 1)) +
        labs(x = "Spiel", y = "Anzahl", fill = "Anzahl Buben")
    )
    
    
    output$Spielwert_plot2 <- renderPlot(
      ggplot(data = Spiele_filtered(), aes_string(x = Spiele_filtered()$Spiel.Buben, fill = Spiele_filtered()$Spieler)) +
        geom_histogram(bins = 16, binwidth = 1) +
        scale_x_continuous(breaks = seq(from = -4, to = 4, by = 1)) +
        labs(x = "Spiel", y = "Anzahl", fill = "Spieler")
    )
  #}
}



eval <- shinyApp(ui = ui, server = server)

eval

#GRF Auswertung-----

Y <- Skat$Gewonnen
X <- Skat[,c(2:5)]
X$Anzahl.Buben <- as.numeric(X$Anzahl.Buben)

RF <- regression_forest(X = X, 
                        Y = Y, 
                        tune.parameters = "all",
                        seed = 541)

RF$tunable.params
RF$tuning.output$grid

print(RF$tuning.output)

plot(get_tree(RF, 2))

#Shiny App-------

ui <- fluidPage(
  theme = shinythemes::shinytheme('darkly'),
  titlePanel("Uljamins Skatrunde"),
  tabsetPanel(
    tabPanel("Datei Upload",
             fileInput('Upload', 'Upload einer xlsx Datei',
                       accept = c(".xlsx"))),
     uiOutput("Verlauf_UI")
    
)
)


server <- function(
  input,
  output,
  session
){
  library(ggplot2)
  library(dplyr)
  library(reshape)
  library(openxlsx)
  
  Skat <- reactive({
    if (is.null(input$Upload)) {
      return(NULL)
    }
    read.xlsx(input$Upload$datapath)
  })
  
  
  #Skat <- reactive(input$Upload$datapath
  #  )
  #
  #if(is.null(Skat)) {
  #  return(NULL)
  #} else{  
  
  Skat()[is.na(Skat())] <- 0
  
  colnames(Skat())[11:13] <- c("Jan", "Marv", "Till")
  Skat()$Gewonnen <- ifelse(Skat()$`Faktor.Sieg/Niederlage` > 0, 1, 0)
  Skat()$Faktor.Spiel[Skat()$Faktor.Spiel == 0] <- 1
  
  Skat()$Anzahl.Buben <- as.factor(Skat()$Anzahl.Buben)
  
  output$Verlauf_UI <- renderUI({
    if (is.null(SKat)) {
      return(NULL)
    }
    
    tabPanel("Verlaufsgrafik",
             pickerInput("sel_Season",
                         "Waehle eine Season",
                         multiple = F,
                         selected = "1",
                         choices = sort(unique(Skat$Season)),
                         options = list(`actions-box` = TRUE)),
             pickerInput("sel_Spieler",
                         "Waehle einen/mehrere Spieler",
                         multiple = T,
                         selected = c("Jan","Marv","Till"),
                         choices = c("Jan", "Marv", "Till"),
                         options = list(`actions-box` = TRUE)),
             mainPanel(p(
               plotOutput("Verlauf_plot")
             )
             )
    )
    
  })
  
  reactive({
    if (is.null(SKat)) {
      return(NULL)
    }
  
  
  filtered_data <- reactive(Skat() %>% 
                              filter(Season == input$sel_Season) %>%
                              melt(id.vars = "Anzahl", measure.vars = c("Jan", "Marv", "Till")) %>%
                              filter(variable %in% input$sel_Spieler))
  output$Verlauf_plot <- renderPlot(
    ggplot(data = filtered_data(), 
           aes_string(x = filtered_data()$Anzahl, y = filtered_data()$value, col = filtered_data()$variable))+
      geom_point(size = 2)+
      geom_path(size = 1) +
      labs(x = "Anzahl", y = "Punktzahl", colour = "Spieler") +
      xlim(c(0,max(filtered_data()$Anzahl + 1))) + 
      ylim(c(min(filtered_data()$value -10),max(filtered_data()$value) + 10))
  )
  output$exp_text <- renderText(paste("some", "text"))
  })
}

shinyApp(ui = ui, server = server)


#Temp Save--------



shinyUI(
  
  fluidPage(
    #shinythemes::themeSelector(),
    theme = shinythemes::shinytheme("darkly"),
    titlePanel("Uljamins Skatrunde"),
    tabsetPanel(
      tabPanel("Datei Upload",
               fileInput('Upload', 'Upload einer xlsx Datei',
                         accept = c(".xlsx"))),
      tabPanel("Verlaufsgrafik",
               pickerInput("sel_Season",
                           "Waehle eine Season",
                           multiple = F,
                           selected = "1",
                           choices = sort(unique(Skat$Season)),
                           options = list(`actions-box` = TRUE)),
               pickerInput("sel_Spieler",
                           "Waehle einen/mehrere Spieler",
                           multiple = T,
                           selected = c("Jan","Marv","Till"),
                           choices = c("Jan", "Marv", "Till"),
                           options = list(`actions-box` = TRUE)),
               mainPanel(p(
                 plotOutput("Verlauf_plot"),
                 br(),
                 p("Der gloriose Verlaufs Graph, jetzt fuer alle Spieler und alle Seasons erhaeltlich in Ihrer lokalen Filiale!")
               ))
      ),
      tabPanel("Auswertungen",
               pickerInput("sel_Kennzahl",
                           "Waehle eine Kennzahl",
                           selected = "Geholte_Punkte",
                           choices = c("Geholte_Punkte", "Gewonnene_Spiele", "Verlorene_Spiele", "Anzahl_Spiele", "Maximale_Punkte", "Minimale_Punkte", "Punkte_pro_Spiel", "Punkte_pro_Sieg", "Punkte_pro_Niederlage", "Siegquote"),
                           options = list(`actions-box` = TRUE)),
               pickerInput("sel_Spieler_Kennzahl",
                           "Waehle einen/mehrere Spieler",
                           multiple = T,
                           selected = c("Jan","Marv","Till"),
                           choices = c("Jan", "Marv", "Till"),
                           options = list(`actions-box` = TRUE)),
               mainPanel(p(
                 plotOutput("Kennzahl_plot"),
                 br()#,
                 #p("Die praezisesten Auswertungen des Sued-Sudans, zertifiziert vom groÃŸen Uljamin persoenlich!")
               ))
      ),
      tabPanel("Faktoren",
               pickerInput("sel_Season_Faktor",
                           "Waehle eine Season",
                           multiple = T,
                           selected = "1",
                           choices = sort(unique(Skat$Season)),
                           options = list(`actions-box` = TRUE)),
               pickerInput("sel_Spieler_Faktor",
                           "Waehle einen/mehrere Spieler",
                           multiple = T,
                           selected = c("Jan", "Marv", "Till"),
                           choices = c("Jan", "Marv", "Till"),
                           options = list(`actions-box` = TRUE)),
               pickerInput("sel_Gewonnen_Faktor",
                           "Waehle zwischen gewonnenen/verlorenen Spielen (1=G, 0=V)",
                           multiple = T,
                           selected = c("1","0"),
                           choices = c("1","0"),
                           options = list(`actions-box` = TRUE)),
               mainPanel(p(
                 p("Histogramm fuer Spielfaktor, aufgeschluesselt nach Spielern"),
                 br(),
                 plotOutput("Faktor_plot_individual")
               ))
      ),
      tabPanel("Spiele",
               pickerInput("sel_Season_Spiele",
                           "Waehle eine Season",
                           multiple = T,
                           selected = "1",
                           choices = sort(unique(Skat$Season)),
                           options = list(`actions-box` = TRUE)),
               pickerInput("sel_Spieler_Spiele",
                           "Waehle einen/mehrere Spieler",
                           multiple = T,
                           selected = c("Jan", "Marv", "Till"),
                           choices = c("Jan", "Marv", "Till"),
                           options = list(`actions-box` = TRUE)),
               pickerInput("sel_Spiel_Spiele",
                           "Waehle ein/mehrere Spiele",
                           multiple = T,
                           selected = c("-4","-3","-2","-1","0","1","2","3","4"),
                           choices = c("-4","-3","-2","-1","0","1","2","3","4"),
                           options = list(`actions-box` = TRUE)),
               mainPanel(p(
                 p("Histogram fuer Spielwerte, aufgeschluesselt nach Anzahl der Buben waehrend des Spiels"),
                 br(),
                 plotOutput("Spielwert_plot"),
                 br(),
                 p("Histogram fuer Spielwerte, aufgeschluesselt nach Spieler"),
                 plotOutput("Spielwert_plot2")
               ))
      )
    )
  )
  
)



#Stackoverflow Code-------

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

#ui.r
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("disableTab","enableTab")),
  inlineCSS(css),
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel( id="tabset",
                   tabPanel("Upload data", value="tab0",
                            fileInput("file1", "Choose CSV File",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            fileInput("file2", "Choose CSV File",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv"))),
                   tabPanel("Resource Allocation", value="tab1"),
                   tabPanel("Time Series", value="tab2")
      )
    )
  )
)
#server.r

server = function(input, output) {
  print("test")
  js$disableTab("tab1")
  js$disableTab("tab2")
  
  observe({
    req(input$file1, input$file2)
    js$enableTab("tab1")
    js$enableTab("tab2")
  })
}

shinyApp(ui, server)
