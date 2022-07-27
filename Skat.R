#Skat Shiy App
#Preliminary Stuff-------
setwd("C:/Users/Nutzer/Desktop/Skat")

#install.packages("shinythemes")
#install.packages("data.table")


library(openxlsx)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DBI)
library(reshape)
library(shinyWidgets)
library(grf)

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
    tabPanel("Verlaufsgrafik",
      pickerInput("sel_Season",
                  "Wähle eine Season",
                  multiple = F,
                  selected = "1",
                  choices = sort(unique(Skat$Season)),
                  options = list(`actions-box` = TRUE)),
      pickerInput("sel_Spieler",
                  "Wähle einen/mehrere Spieler",
                  multiple = T,
                  selected = c("Jan","Marv","Till"),
                  choices = c("Jan", "Marv", "Till"),
                  options = list(`actions-box` = TRUE)),
    mainPanel(p(
      plotOutput("Verlauf_plot"),
      br(),
      p("Der gloriose Verlaufs Graph, jetzt für alle Spieler und alle Seasons erhältlich in Ihrer lokalen Filiale!")
    ))
    ),
    tabPanel("Auswertungen",
             pickerInput("sel_Kennzahl",
                        "Wähle eine Kennzahl",
                        selected = "Geholte_Punkte",
                        choices = c("Geholte_Punkte", "Gewonnene_Spiele", "Verlorene_Spiele", "Anzahl_Spiele", "Maximale_Punkte", "Minimale_Punkte", "Punkte_pro_Spiel", "Punkte_pro_Sieg", "Punkte_pro_Niederlage", "Siegquote"),
                        options = list(`actions-box` = TRUE)),
             pickerInput("sel_Spieler_Kennzahl",
                        "Wähle einen/mehrere Spieler",
                        multiple = T,
                        selected = c("Jan","Marv","Till"),
                        choices = c("Jan", "Marv", "Till"),
                        options = list(`actions-box` = TRUE)),
            mainPanel(p(
              plotOutput("Kennzahl_plot"),
              br(),
              p("Die präzisesten Auswertungen des Süd-Sudans, zertifiziert vom großen Uljamin persönlich!")
            ))
  ),
  tabPanel("Faktoren",
           pickerInput("sel_Season_Faktor",
                       "Wähle eine Season",
                       multiple = T,
                       selected = "1",
                       choices = sort(unique(Skat$Season)),
                       options = list(`actions-box` = TRUE)),
           pickerInput("sel_Spieler_Faktor",
                       "Wähle einen/mehrere Spieler",
                       multiple = T,
                       selected = c("Jan", "Marv", "Till"),
                       choices = c("Jan", "Marv", "Till"),
                       options = list(`actions-box` = TRUE)),
           pickerInput("sel_Gewonnen_Faktor",
                       "Wähle zwischen gewonnenen/verlorenen Spielen (1=G, 0=V)",
                       multiple = T,
                       selected = c("1","0"),
                       choices = c("1","0"),
                       options = list(`actions-box` = TRUE)),
           mainPanel(p(
             p("Histogramm für Spielfaktor, aufgeschlüsselt nach Spielern"),
             br(),
             plotOutput("Faktor_plot_individual")
           ))
  ),
  tabPanel("Spiele",
           pickerInput("sel_Season_Spiele",
                       "Wähle eine Season",
                       multiple = T,
                       selected = "1",
                       choices = sort(unique(Skat$Season)),
                       options = list(`actions-box` = TRUE)),
           pickerInput("sel_Spieler_Spiele",
                       "Wähle einen/mehrere Spieler",
                       multiple = T,
                       selected = c("Jan", "Marv", "Till"),
                       choices = c("Jan", "Marv", "Till"),
                       options = list(`actions-box` = TRUE)),
           pickerInput("sel_Spiel_Spiele",
                       "Wähle ein/mehrere Spiele",
                       multiple = T,
                       selected = c("-4","-3","-2","-1","0","1","2","3","4"),
                       choices = c("-4","-3","-2","-1","0","1","2","3","4"),
                       options = list(`actions-box` = TRUE)),
           mainPanel(p(
             p("Histogram für Spielwerte, aufgeschlüsselt nach Anzahl der Buben während des Spiels"),
             br(),
             plotOutput("Spielwert_plot"),
             br(),
             p("Histogram für Spielwerte, aufgeschlüsselt nach Spieler"),
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
  Skat
  
  
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
  
}



shinyApp(ui = ui, server = server)


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
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme('superhero'),
  titlePanel("Uljamins Skatrunde"),
  sidebarLayout(
    sidebarPanel(p(
      selectInput("sel_Season",
                  "Wähle eine Season",
                  multiple = F,
                  selected = "1",
                  choices = sort(unique(Skat$Season))),
      selectInput("sel_Spieler",
                  "Wähle einen/mehrere Spieler",
                  multiple = T,
                  selected = c("Jan","Marv","Till"),
                  choices = c("Jan", "Marv", "Till")),
    )),
    mainPanel(p(
      plotOutput("Verlauf_plot")
    )
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
  Skat
  
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
}

shinyApp(ui = ui, server = server)


#Temp Save--------

output$Faktor_plot_total <- renderPlot(
  ggplot(data = filtered_faktor_data(), aes_string(x = filtered_faktor_data()$Faktor.Spiel)) +
    geom_histogram(bins = 16, binwidth = 0.038, origin = -0.019) +
    labs(x = "Faktor", y = "Anzahl")
)




#Stackoverflow Code-------

