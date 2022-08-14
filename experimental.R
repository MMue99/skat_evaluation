#Experimental File

#packages---------
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

#jshiny code-----
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

#ui--------
ui <- fluidPage(
  #jshiny
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

#server--------
server <- function(
  input,
  output,
  session
){
  library(ggplot2)
  library(dplyr)
  library(reshape)
  library(openxlsx)
  ##jshiny code to disable panels---------
  js$disableTab("Verlaufsgrafik")
  js$disableTab("Auswertungen")
  js$disableTab("Faktoren")
  js$disableTab("Spiele")
  
  observe({
    req(input$Upload)
    js$enableTab("Verlaufsgrafik")
    js$enableTab("Auswertungen")
    js$enableTab("Faktoren")
    js$enableTab("Spiele")
  })
  
  ##reading in data--------
  df <- reactive({
    check <- input$Upload
    req(check)
    
    read.xlsx(input$Upload$datapath)
  })
  
  output$contents <- renderTable({
    Skat <- df()
  })
  
  ##UI Rendering-------
  ###Season-Verlauf------
  output$Season_input <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Season",
                "Waehle eine Season",
                multiple = F,
                selected = "1",
                choices = sort(unique(Skat$Season)),
                options = list(`actions-box` = TRUE))
  })
  
  ###Spieler-Verlauf-------
  output$Spieler_input <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Spieler",
                "Waehle einen/mehrere Spieler",
                multiple = T,
                selected = c("Jan","Marv","Till"),
                choices = c("Jan", "Marv", "Till"),
                options = list(`actions-box` = TRUE))
  })
  
  ###Spieler-Kennzahl-------
  output$Spieler_Kennzahl <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Spieler_Kennzahl",
                "Waehle einen/mehrere Spieler",
                multiple = T,
                selected = c("Jan","Marv","Till"),
                choices = c("Jan", "Marv", "Till"),
                options = list(`actions-box` = TRUE))
  })
  
  ###Kennzahl-Kennzahl------
  output$Kennzahl_Kennzahl <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Kennzahl",
                "Waehle eine Kennzahl",
                selected = "Geholte_Punkte",
                choices = c("Geholte_Punkte", "Gewonnene_Spiele", "Verlorene_Spiele", "Anzahl_Spiele", "Maximale_Punkte", "Minimale_Punkte", "Punkte_pro_Spiel", "Punkte_pro_Sieg", "Punkte_pro_Niederlage", "Siegquote"),
                options = list(`actions-box` = TRUE))
  })
  
  ###Season-Faktor--------
  output$Season_Faktor <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Season_Faktor",
                "Waehle eine Season",
                multiple = T,
                selected = "1",
                choices = sort(unique(Skat$Season)),
                options = list(`actions-box` = TRUE))
  })
  
  ###Spieler-Faktor------
  output$Spieler_Faktor <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Spieler_Faktor",
                "Waehle einen/mehrere Spieler",
                multiple = T,
                selected = c("Jan", "Marv", "Till"),
                choices = c("Jan", "Marv", "Till"),
                options = list(`actions-box` = TRUE))
  })
  
  ###Gewonnen-Faktor------
  output$Gewonnen_Faktor <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Gewonnen_Faktor",
                "Waehle zwischen gewonnenen/verlorenen Spielen (1=G, 0=V)",
                multiple = T,
                selected = c("1","0"),
                choices = c("1","0"),
                options = list(`actions-box` = TRUE))
  })
  
  ###Season-Spiele-----
  output$Season_Spiele <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Season_Spiele",
                "Waehle eine Season",
                multiple = T,
                selected = "1",
                choices = sort(unique(Skat$Season)),
                options = list(`actions-box` = TRUE))
  })
  
  ###Spieler-Spiele--------
  output$Spieler_Spiele <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Spieler_Spiele",
                "Waehle einen/mehrere Spieler",
                multiple = T,
                selected = c("Jan", "Marv", "Till"),
                choices = c("Jan", "Marv", "Till"),
                options = list(`actions-box` = TRUE))
  })
  
  ###Spiel-Spiele-----
  output$Spiel_Spiele <- renderUI({
    if (is.null(df())) {
      return(NULL)
    }
    Skat <- df()
    pickerInput("sel_Spiel_Spiele",
                "Waehle ein/mehrere Spiele",
                multiple = T,
                selected = c("-4","-3","-2","-1","0","1","2","3","4"),
                choices = c("-4","-3","-2","-1","0","1","2","3","4"),
                options = list(`actions-box` = TRUE))
  })
  
  ##Verlaufsgrafik-------
  ###filtered data für verlaufsgrafik-----
  filtered_data <- reactive({
      check <- input$Upload
      req(check)
      Skat <- df()
      
      Skat[is.na(Skat)] <- 0
      colnames(Skat)[11:13] <- c("Jan", "Marv", "Till")
      Skat$Gewonnen <- ifelse(Skat$`Faktor.Sieg/Niederlage` > 0, 1, 0)
      Skat$Faktor.Spiel[Skat$Faktor.Spiel == 0] <- 1
      Skat$Anzahl.Buben <- as.factor(Skat$Anzahl.Buben)
      
      Skat %>%
        filter(Season == input$sel_Season) %>%
        melt(id.vars = "Anzahl", measure.vars = c("Jan", "Marv", "Till")) %>%
        filter(variable %in% input$sel_Spieler)
  
    
  })#End of reactive
    
    
   ###Verlaufsplot------- 
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
    
  #}#End of if
  
    ##Kennzahlseite------
    ###Kennzahl Data---------
      Win_rate_filtered <- reactive({
        check <- input$Upload
        req(check)
        WR_working <- df()
        
        WR_working[is.na(WR_working)] <- 0
        colnames(WR_working)[11:13] <- c("Jan", "Marv", "Till")
        WR_working$Gewonnen <- ifelse(WR_working$`Faktor.Sieg/Niederlage` > 0, 1, 0)
        WR_working$Faktor.Spiel[WR_working$Faktor.Spiel == 0] <- 1
        WR_working$Anzahl.Buben <- as.factor(WR_working$Anzahl.Buben)
        
        WR_working <- WR_working %>% 
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
      
        WR_working$Spieler <- ifelse(WR_working$Spieler == 1, "Jan", ifelse(WR_working$Spieler == 2, "Marv", "Till"))
        WR_working$Season <- as.numeric(WR_working$Season)
        WR_working$Spieler <- as.factor(WR_working$Spieler)
        WR_working[is.na(WR_working)] <- 0
        
        WR_working %>%
          filter(Spieler %in% input$sel_Spieler_Kennzahl)
      
      })#End of reactive
      
      ###Kennzahl Plot-----
      output$Kennzahl_plot <- renderPlot(
        ggplot(data = Win_rate_filtered(), aes_string(x = Win_rate_filtered()$Season, y = input$sel_Kennzahl, col = Win_rate_filtered()$Spieler)) +
          geom_line(size = 1) + 
          geom_point(size = 2) +
          labs(x = "Season", colour = "Spieler") +
          scale_x_continuous(breaks = sort(unique(Win_rate_filtered()$Season)))
      )
      
      ##Faktorenseite----
      ###Faktoren Data-------
      filtered_faktor_data <- reactive({
        check <- input$Upload
        req(check)
        fd_working <- df()
        
        fd_working[is.na(fd_working)] <- 0
        colnames(fd_working)[11:13] <- c("Jan", "Marv", "Till")
        fd_working$Gewonnen <- ifelse(fd_working$`Faktor.Sieg/Niederlage` > 0, 1, 0)
        fd_working$Faktor.Spiel[fd_working$Faktor.Spiel == 0] <- 1
        fd_working$Anzahl.Buben <- as.factor(fd_working$Anzahl.Buben)
        
        fd_working$Spieler <- ifelse(fd_working$Spieler == 1, "Jan", ifelse(fd_working$Spieler == 2, "Marv", "Till"))
        fd_working$Spieler <- as.factor(fd_working$Spieler)
        
        fd_working %>%
          filter(Spieler %in% input$sel_Spieler_Faktor) %>%
          filter(Season %in% input$sel_Season_Faktor) %>%
          filter(Gewonnen %in% input$sel_Gewonnen_Faktor)
      })
      
      ###Faktor Plot---------
      output$Faktor_plot_individual <- renderPlot(
        ggplot(data = filtered_faktor_data(), aes_string(x = filtered_faktor_data()$Faktor.Spiel, fill = filtered_faktor_data()$Spieler)) +
          geom_histogram(bins = 16, binwidth = 0.038, origin = -0.019) +
          labs(x = "Faktor", y = "Anzahl", fill = "Spieler")
      )
      
      ##Spieleseite-----
      ###Spiele Data-----
      Spiele_filtered <- reactive({
        check <- input$Upload
        req(check)
        sp_working <- df()
        
        sp_working[is.na(sp_working)] <- 0
        colnames(sp_working)[11:13] <- c("Jan", "Marv", "Till")
        sp_working$Gewonnen <- ifelse(sp_working$`Faktor.Sieg/Niederlage` > 0, 1, 0)
        sp_working$Faktor.Spiel[sp_working$Faktor.Spiel == 0] <- 1
        sp_working$Anzahl.Buben <- as.factor(sp_working$Anzahl.Buben)
        
        sp_working$Spieler <- ifelse(sp_working$Spieler == 1, "Jan", ifelse(sp_working$Spieler == 2, "Marv", "Till"))
        sp_working$Spieler <- as.factor(sp_working$Spieler)
        
        sp_working %>%
          filter(Spieler %in% input$sel_Spieler_Spiele) %>%
          filter(Season %in% input$sel_Season_Spiele) %>%
          filter(Spiel.Buben %in% input$sel_Spiel_Spiele)
      
      })
      
      ###Spiele Plot-------
      ####Plot 1----------
      output$Spielwert_plot <- renderPlot(
        ggplot(data = Spiele_filtered(), aes_string(x = Spiele_filtered()$Spiel.Buben, fill = Spiele_filtered()$Anzahl.Buben)) +
          geom_histogram(bins = 16, binwidth = 1) +
          scale_x_continuous(breaks = seq(from = -4, to = 4, by = 1)) +
          labs(x = "Spiel", y = "Anzahl", fill = "Anzahl Buben")
      )
      
      ####Plot 2------
      output$Spielwert_plot2 <- renderPlot(
        ggplot(data = Spiele_filtered(), aes_string(x = Spiele_filtered()$Spiel.Buben, fill = Spiele_filtered()$Spieler)) +
          geom_histogram(bins = 16, binwidth = 1) +
          scale_x_continuous(breaks = seq(from = -4, to = 4, by = 1)) +
          labs(x = "Spiel", y = "Anzahl", fill = "Spieler")
      )
      
}

#App ausführe--------
shinyApp(ui = ui, server = server)
