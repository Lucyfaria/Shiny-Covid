
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(osmdata)   # Access to Open Street Map data
library(sf)        # Simple Features (data frame with geometries)  
library(plotly)


RKI <- read_csv(file ="https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv")
RKI2 <- read_csv('https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid-19 Daten"),
    
    selectInput("state", label = h3("Bundesland:"), 
                choices = c('Schleswig-Holstein','Hamburg','Niedersachsen','Nordrhein-Westfalen',
                            'Hessen','Rheinland-Pfalz','Baden-Württemberg','Bayern','Saarland',
                            'Brandenburg','Mecklenburg-Vorpommern','Sachsen','Sachsen-Anhalt',
                            'Thüringen','Berlin','Bremen'),
                selected = 1),
    
    wellPanel(selectInput('sort',label = h3('Tagesaktuelle Zahlen'), choices = c('Alphabetisch','Aufsteigend','Absteigend'),selected = 1),
    
    plotlyOutput('Plot'),
    p(strong('Die Linien in dem Diagramm stellen die Grenzwerte dar.
              Der Grenzwert 1 liegt immer bei 50 Personen kummuliert von den Neuinfektionen der letzten 7 Tage. 
              Bei diesem Wert ist es noch möglich die Neuinfizierte zu kontaktieren und die Kontaktpersonen zu ermitteln.
              Bei Überschreiten des zweiten Grenzwertes von 200 wird empfohlen härtere Maßnahmen zu ergreifen. Bspw. eine Ausgangssperre zu verhängen'))),
    
    wellPanel(dateRangeInput("dates", label = h3("Zeitreihe seit Beginn der Pandemie"), start = '2020-01-01'),
    
    plotOutput('Plot2'))
   # DT::dataTableOutput("table")
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    
    # output$table <- DT::renderDataTable(DT::datatable({
    
    
    
    output$Plot <- renderPlotly({
     
      switch(input$sort,
      'Absteigend'= {
        
        
          x<- RKI %>%
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
          theme(axis.text.x = element_text(size= 9,angle = 90, hjust = 1, vjust = 0.2), axis.text.y = element_text(size = 9))+
          scale_color_hue(labels =  "Limit")
          
          ggplotly(x, tooltip = 'text') %>% layout(height = 800, width = 800)
      },
      'Alphabetisch' = {
      
        
          y<-RKI %>%
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
          theme(axis.text.x = element_text(size= 9,angle = 90, hjust = 1, vjust = 0.2), axis.text.y = element_text(size = 9))+
          scale_color_hue(labels =  "Limit")
          
          ggplotly(y, tooltip = 'text') %>% layout(height = 800, width = 800)
      },
      'Aufsteigend' = {
        
        
          z<-RKI %>%
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
          theme(axis.text.x = element_text(size= 9,angle = 90, hjust = 1, vjust = 0.2), axis.text.y = element_text(size = 9))+
          scale_color_hue(labels =  "Limit")
          
          ggplotly(z, tooltip = 'text') %>% layout(height = 800, width = 800)
      }
               
               )
      
     
    })
   # }))

    
     output$Plot2 <- renderPlot({
       
       RKI2$Meldedatum <- as.Date(RKI2$Meldedatum, "%Y/%m/%d %H:%M:%S")
       
       RKI2 %>%
         filter(Bundesland %in% input$state) %>%
         filter(Meldedatum >= input$dates[1] & Meldedatum <= input$dates[2]) %>%
         ggplot() +
         aes(x = Meldedatum, weight = AnzahlFall) +
         labs(x = "Meldedatum",y = "Neuer Fall") +
         geom_bar(fill = "#0c4c8a") +
         theme_minimal()
       
       
       
       # RKI2 %>%
       #   filter(Bundesland %in% input$state) %>%
       #   filter(Meldedatum >=
       #            input$dates[1] & Meldedatum <= input$dates[2]) %>%
       #   ggplot() +
       #   aes(x = Meldedatum, weight = AnzahlFall) +
       #   geom_bar(fill = "#0c4c8a") +
       #   theme_minimal()
     })
    
      
# TODO: Axenbeschriftungen und Date Group by
    
}

# Run the application 
shinyApp(ui = ui, server = server)
