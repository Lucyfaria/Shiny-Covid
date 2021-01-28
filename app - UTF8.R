# Load Libraries
library(shiny)
library(readr)
library(tidyverse)
library(osmdata)   # Access to Open Street Map data
library(sf)        # Simple Features (data frame with geometries)  
library(plotly)
library(shinythemes)

# Read in Data
RKI <- read_csv(file ="https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv")
#RKI2 <- read_csv('https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv')
RKI2 <- read_csv("C:/Users/Nina/Downloads/RKI_COVID19 (1).csv")

# Prepare Data
RKI3<- RKI2 %>% 
  group_by(Meldedatum,Bundesland) %>% 
  summarise(AnzahlFall =sum(AnzahlFall, na.rm = T))

RKI3$Meldedatum <- as.Date(RKI3$Meldedatum, "%Y/%m/%d %H:%M:%S")

RKI4<- RKI2 %>% 
  group_by(Altersgruppe,Geschlecht,Bundesland) %>% 
  summarise(AnzahlFall =sum(AnzahlFall, na.rm = T))

RKI5<- RKI2 %>% 
  group_by(Altersgruppe,Geschlecht,Bundesland) %>% 
  summarise(AnzahlTodesfall =sum(AnzahlTodesfall, na.rm = T))

# Define UI for application 
ui <- fluidPage(
  theme = shinytheme( 'superhero'),
  
  titlePanel("Covid-19 Daten"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("state", label = h3("Bundesland:"), 
                  choices = c('Schleswig-Holstein','Hamburg','Niedersachsen','Bremen','Nordrhein-Westfalen',
                              'Hessen','Rheinland-Pfalz','Baden-Württemberg','Bayern','Saarland','Berlin',
                              'Brandenburg','Mecklenburg-Vorpommern','Sachsen','Sachsen-Anhalt',
                              'Thüringen'),
                  selected = 1),
      p('Die Reihenfolge der Bundesländer entspricht der Reihenfolge der zugehörigen Nummern der Bundesländer.  01-Schleswig-Holstein, 02-Hamburg usw.')
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
                            Bei Überschreiten des zweiten Grenzwertes von 200 wird empfohlen härtere Maßnahmen zu ergreifen bspw. eine Ausgangssperre zu verhängen.')),
        
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
                 
                 p(strong('Dieser Plot zeigt die Anzahl der Infizierten pro Bundesland seit Beginn der Pandemie.
                          Durch den Datumsregler kann der Zeitraum eingegrenzt werden. Eine Trendlinie kann durch die Select-Box optional auf den Plot gelegt werden.')),
                 
                 plotlyOutput('Plot2')),
        
        tabPanel("Alter/Geschlecht",
                 p(strong('Altersverteilung seit Beginn der Pandemie pro Bundesland')),
                 plotlyOutput('Plot3'),
                 plotlyOutput('Plot4')),
        
        tabPanel("Infos",
                 fluidRow(
                   wellPanel(
                     column(width = 12,
                            p(strong('Tagesaktuelle Daten')),
                            p('Die Tagesaktuellen Daten stammen vom Robert-Koch-Institut und werden täglich aktualisiert.'),
                            uiOutput("Link1")
                            ),
                            column(8,p('   ')),
                            column(6,
                            p('Daten vom Tagesaktuellen Plot wurden heruntergeladen am:  ',textOutput("currentDate"))))),
                 fluidRow(
                   wellPanel(
                     column(4,
                            p(strong('Zeitreihendaten')),
                            p('Die Zeitreihendaten stammen vom Robert-Koch-Institut und werden täglich aktualisiert.'),
                            uiOutput("Link2")))),
                 fluidRow(
                   wellPanel(
                     column(width= 12,
                            p(strong('Github')),
                            p('Der Code dieser Webseite wurde auf Github veröffentlicht'),
                            uiOutput('GLink'))))
                 
                 
           )
        )
        
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
    output$currentDate <- renderText({
    
     # format(Sys.time(), "%d.%b %Y")
      print(RKI$last_update[1])
  })
  
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
        labs(x = "Meldedatum",y = "Anzahl der Infizierten pro Bundesland") +
        geom_col(size=1L,colour = "#0c4c8a") +
        theme_minimal()
      
      ggplotly(ZeitPlotA, tooltip = 'text') %>% layout(height = 450)
             
      }, 'Ja' = {
       
        ZeitPlotB<- RKI3%>%
         filter(Bundesland == input$state) %>%
         filter(Meldedatum >= input$dates[1] & Meldedatum <= input$dates[2]) %>%
         ggplot() +
         aes(x = Meldedatum, y = AnzahlFall) +
         labs(x = "Meldedatum", y = " Anzahl der Infizierten pro Bundesland") +
         geom_col(size = 1L, colour = "#0c4c8a") +
         geom_smooth(se = FALSE, span = 0.25, colour ="#6baed6") +
         theme_minimal()
       
        
        ggplotly(ZeitPlotB, tooltip = 'NULL') %>% layout(height = 450)
       
     })
    })
    
    output$Plot3 <- renderPlotly({
      
     
      AgeFall<- RKI4 %>%
        filter(!(Altersgruppe %in% "unbekannt")) %>%
        filter(!(Geschlecht %in% "unbekannt")) %>%
        
        filter(Bundesland %in% input$state) %>%
        ggplot() +
        aes(x = Altersgruppe, fill = Geschlecht, weight = AnzahlFall,text = paste0("Geschlecht: ",Geschlecht, "\n", "Anzahl Fälle: ", AnzahlFall)) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Set1", direction = -1) +
        scale_x_discrete(labels = c("0-4", "5-14", "15-34", "35-59", "60-79","80+")) +
        labs(x = "Altersgruppe", y = "Kummulierte Infizierte", title = "Kummulierte Fälle der gemeldeten Infizierten", subtitle = input$state, fill = "Geschlecht") +
        theme_minimal() +
        theme(legend.position = "none")
      
        ggplotly(AgeFall, tooltip = 'text') %>% layout(height = 300)
        
    })
    
    output$Plot4 <- renderPlotly({
      
      AgeTod<-RKI5 %>%
        filter(!(Altersgruppe %in% "unbekannt")) %>%
        filter(!(Geschlecht %in% "unbekannt")) %>%
        
        filter(Bundesland %in% input$state) %>%
        ggplot() +
        aes(x = Altersgruppe, fill = Geschlecht, weight = AnzahlTodesfall,text = paste0("Geschlecht: ",Geschlecht, "\n", "Anzahl Todesälle: ", AnzahlTodesfall)) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Reds", direction = 1) +
        scale_x_discrete(labels = c("0-4", "5-14", "15-34", "35-59", "60-79","80+"))+
        labs(x = "Altersgruppe", y = "Kummulierte Todesfälle", title = "Kummulierte Fälle der mit oder an Covid-19 Verstorbenen", subtitle = input$state, fill = "Geschlecht") +
        
        theme_minimal()+
        theme(legend.position = "none")
      
      ggplotly(AgeTod, tooltip = 'text') %>% layout(height = 300)
    })
    
    url <- a("Tagesdaten", href="https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0", target="_blank")
    output$Link1 <- renderUI({
      tagList("Link:", url)
    })
    
    url2 <- a("Zeitreihendaten", href="https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0", target="_blank")
    output$Link2 <- renderUI({
      tagList("Link:", url)
    })
    
    url3 <- a("Github", href="https://github.com/Lucyfaria/Shiny-Covid", target="_blank")
    output$GLink <- renderUI({
      tagList("Link:", url)
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
