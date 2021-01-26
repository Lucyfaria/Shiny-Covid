
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(osmdata)   # Access to Open Street Map data
library(sf)        # Simple Features (data frame with geometries)  
library(plotly)
library(shinythemes)


RKI <- read_csv(file ="https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv")
#RKI2 <- read_csv('https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv')
RKI2 <- read_csv("C:/Users/Nina/Downloads/RKI_COVID19 (1).csv")

RKI3<- RKI2 %>% 
  group_by(Meldedatum,Bundesland) %>% 
  summarise(AnzahlFall =sum(AnzahlFall, na.rm = T))

RKI3$Meldedatum <- as.Date(RKI3$Meldedatum, "%Y/%m/%d %H:%M:%S")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme( 'superhero'),
  
  titlePanel("Covid-19 Daten"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("state", label = h3("Bundesland:"), 
                  choices = c('Schleswig-Holstein','Hamburg','Niedersachsen','Nordrhein-Westfalen',
                              'Hessen','Rheinland-Pfalz','Baden-Württemberg','Bayern','Saarland',
                              'Brandenburg','Mecklenburg-Vorpommern','Sachsen','Sachsen-Anhalt',
                              'Thüringen','Berlin','Bremen'),
                  selected = 1),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tagesaktuelle Daten",
                 selectInput('sort',
                             label = h3('Tagesaktuelle Zahlen'), 
                             choices = c('Alphabetisch','Aufsteigend','Absteigend'),
                             selected = 1),
                 
                 p(strong('Die Linien in dem Diagramm stellen die Grenzwerte dar.
                            Der Grenzwert 1 liegt immer bei 50 Personen kummuliert von den Neuinfektionen der letzten 7 Tage. 
                            Bei diesem Wert ist es noch möglich die Neuinfizierte zu kontaktieren und die Kontaktpersonen zu ermitteln.
                            Bei Überschreiten des zweiten Grenzwertes von 200 wird empfohlen härtere Maßnahmen zu ergreifen bspw. eine Ausgangssperre zu verhängen')),
        
                 plotlyOutput('Plot')),
     
       
      
        tabPanel("Zeitreihendaten", 
                 dateRangeInput("dates", 
                                label = h3("Zeitreihe seit Beginn der Pandemie"), 
                                separator = " bis ",
                                start = min(RKI2$Meldedatum, na.rm = TRUE)),
                 selectInput('smooth',
                             label = h3('Trendlinie anzeigen'), 
                             choices = c('Nein','Ja'),
                             selected = 1),
                 
                 plotlyOutput('Plot2')))
        
      )
    )
)


    # Application title
    #titlePanel("Covid-19 Daten"),
    
    
    
    #wellPanel(
    
    
    #wellPanel(
   # DT::dataTableOutput("table")
    
    
    
#)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # output$table <- DT::renderDataTable(DT::datatable({
    
    output$Plot <- renderPlotly({
     
      switch(input$sort,
      'Absteigend'= {
        
        
          AbPlot<- RKI %>%
          filter(BL %in% input$state) %>%
          mutate(cases7_per_100k = as.integer(cases7_per_100k)) %>%
          ggplot() +
          aes(x = reorder(county, -cases7_per_100k), weight = cases7_per_100k, text = paste0(county, "\n", "Fälle: ", cases7_per_100k)) +
          geom_bar(fill = "#0c4c8a") +
          #geom_line(data = limit, aes(x=SH.GEN, y= cases7_per_100k, colour = "red", group = 1)) +
          geom_hline(yintercept = 50, colour = 'orange') +
          geom_hline(yintercept = 200, colour = 'red') +
          labs(x = " ",y = "Neuinfektionen pro 100k Einw. (7 Tage)", title = input$state, colour = " ") +
          theme_minimal()+
          theme(axis.text.x = element_text(size= 9,angle = 90, hjust = 1, vjust = 0.2), axis.text.y = element_text(size = 9))
          
          ggplotly(AbPlot, tooltip = 'text') %>% layout(height = 700)
      },
      'Alphabetisch' = {
      
        
          AlpPlot<-RKI %>%
          filter(BL %in% input$state) %>%
          mutate(cases7_per_100k = as.integer(cases7_per_100k)) %>%
          ggplot() +
          aes(x = county, weight = cases7_per_100k, text = paste0(county, "\n", "Fälle: ", cases7_per_100k)) +
          geom_bar(fill = "#0c4c8a") +
          #geom_line(data = limit, aes(x=SH.GEN, y= cases7_per_100k, colour = "red", group = 1)) +
          geom_hline(yintercept = 50, colour = 'orange') +
          geom_hline(yintercept = 200, colour = 'red') +
          labs(x = " ",y = "Neuinfektionen pro 100k Einw. (7 Tage)", title = input$state, colour = " ") +
          theme_minimal()+
          theme(axis.text.x = element_text(size= 9,angle = 90, hjust = 1, vjust = 0.2), axis.text.y = element_text(size = 9))
          
          ggplotly(AlpPlot, tooltip = 'text') %>% layout(height = 700)
      },
      'Aufsteigend' = {
        
        
          AufPlot<-RKI %>%
          filter(BL %in% input$state) %>%
          mutate(cases7_per_100k = as.integer(cases7_per_100k)) %>%
          ggplot() +
          aes(x = reorder(county, cases7_per_100k), weight = cases7_per_100k, text = paste0(county, "\n", "Fälle: ", cases7_per_100k)) +
          geom_bar(fill = "#0c4c8a") +
          #geom_line(data = limit, aes(x=SH.GEN, y= cases7_per_100k, colour = "red", group = 1)) +
          geom_hline(yintercept = 50, colour = 'orange') +
          geom_hline(yintercept = 200, colour = 'red') +
          labs(x = " ",y = "Neuinfektionen pro 100k Einw. (7 Tage)", title = input$state, colour = " ") +
          theme_minimal()+
          theme(axis.text.x = element_text(size= 9,angle = 90, hjust = 1, vjust = 0.2), axis.text.y = element_text(size = 9))
          
          ggplotly(AufPlot, tooltip = 'text') %>% layout(height = 700)
      }
               
               )
      
     
    })
   # }))

    output$Plot2 <- renderPlotly({
      
      switch(input$smooth,
             'Nein' = {
      
      ZeitPlotA<- RKI3%>%
        filter(Bundesland == input$state) %>%
        filter(Meldedatum >= input$dates[1] & Meldedatum <= input$dates[2]) %>%
        ggplot() +
        aes(x = Meldedatum, y = AnzahlFall, text = paste0(input$state, "\n", "Anzahl Fälle: ", AnzahlFall)) +
        labs(x = "Meldedatum",y = "Anzahl Fälle") +
        geom_col(size=1L,colour = "#0c4c8a") +
        theme_minimal()
      
      ggplotly(ZeitPlotA, tooltip = 'text') %>% layout(height = 450)
             
      }, 'Ja' = {
       
        ZeitPlotB<- RKI3%>%
         filter(Bundesland == input$state) %>%
         filter(Meldedatum >= input$dates[1] & Meldedatum <= input$dates[2]) %>%
         ggplot() +
         aes(x = Meldedatum, y = AnzahlFall) +
         labs(x = "Meldedatum", y = "Anzahl Fälle") +
         geom_col(size = 1L, colour = "#0c4c8a") +
         geom_smooth(se = FALSE, span = 0.25, colour ="#6baed6") +
         theme_minimal()
       
        
        ggplotly(ZeitPlotB, tooltip = 'all') %>% layout(height = 450)
       
     })
    })
    
      

    
}

# Run the application 
shinyApp(ui = ui, server = server)
