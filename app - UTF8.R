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
RKI2 <- read_csv('https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv')

KH<- read_csv("https://diviexchange.blob.core.windows.net/%24web/DIVI_Intensivregister_Auszug_pro_Landkreis.csv")
KH <- RKI %>% full_join(KH, by = c("RS" = "gemeindeschluessel"))

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
      p('Die Reihenfolge der Bundesländer entspricht der Reihenfolge der zugehörigen Nummern der Bundesländer.  01-Schleswig-Holstein, 02-Hamburg usw.'),
      htmlOutput('frame')
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
                                format = "dd-mm-yyyy",
                                start = min(RKI2$Meldedatum, na.rm = TRUE)),
                 selectInput('smooth',
                             label = h3('Trendlinie anzeigen'), 
                             choices = c('Nein','Ja'),
                             selected = 1),
                 
                 p(strong('Dieser Plot zeigt die Anzahl der Infizierten pro Bundesland seit Beginn der Pandemie.
                          Durch den Datumsregler kann der Zeitraum eingegrenzt werden. Eine Trendlinie kann durch die Select-Box optional auf den Plot gelegt werden.')),
                 
                 plotlyOutput('Plot2')),
        tabPanel("DVI",
                plotlyOutput('Plot5'),
                br(),
                textOutput('insgesamt'),
                #p('Insgesamt sind ',style="display:inline",textOutput('insgesamt'),'Menschen im Krankenhaus'),
                #p('Es werden',style="display:inline",textOutput('prozent'),'% beatmet'),
                textOutput('prozent'),
                br(),
                p('Sollte der Wert 0 sein, liegen zu diesem Bundesland leider keine spezifischen Daten vor.')),
        
        tabPanel("Alter/Geschlecht",
                 h3(p(strong('Altersverteilung seit Beginn der Pandemie pro Bundesland'))),
                 plotlyOutput('Plot3'),
                 plotlyOutput('Plot4')),
        
        tabPanel("Infos",
                 wellPanel(
                    p(strong('Tagesaktuelle Daten')),
                            p('Die tagesaktuellen Daten stammen vom Robert-Koch-Institut und werden täglich aktualisiert.'),
                            uiOutput("Link1"),
                            br(),
                            p('Die Daten vom tagesaktuellen Plot wurden aktualisiert am: ',textOutput("currentDate"))),
                 
                   wellPanel(
                            p(strong('Zeitreihendaten')),
                            p('Die Zeitreihendaten stammen vom Robert-Koch-Institut und werden täglich aktualisiert.'),
                            uiOutput("Link2")),
                   
                   wellPanel(
                            p(strong('DVI-Daten')),
                            p('Die DVI-Daten stammen von der Webseite des deutschen Intensivregisters.'),
                            uiOutput('Link3')),

                   wellPanel(
                            p(strong('Github')),
                            p('Der Code dieser Webseite wurde auf Github veröffentlicht'),
                            uiOutput('GLink'))
                 
                 
           )
        )
        
      )
    )
)


# Define server logic required
server <- function(input, output) {
  
  output$frame <- renderUI({
    covid <- tags$iframe(src="https://sketchfab.com/models/90d920eaea3643a388b0d5855cc38c27/embed?autospin=0.2&amp;autostart=1&amp;ui_controls=1&amp;ui_infos=1&amp;ui_inspector=1&amp;ui_stop=1&amp;ui_theme=dark&amp;ui_watermark=0&amp;ui_watermark_link=0",
                           style = 'width:100%',
                           frameborder=0)
    print(covid)
    covid
  })
  
    
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
   
    output$Plot2 <- renderPlotly({
      
      switch(input$smooth,
             'Nein' = {
      
      ZeitPlotA<- RKI3%>%
        filter(Bundesland == input$state) %>%
        filter(Meldedatum >= input$dates[1] & Meldedatum <= input$dates[2]) %>%
        ggplot() +
        aes(x = Meldedatum, y = AnzahlFall, text = paste0(input$state, "\n", "Anzahl Fälle: ", AnzahlFall, '\n',Meldedatum)) +
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
    
    output$Plot5 <- renderPlotly({
      KH2 <- KH %>%
        filter(BL %in% input$state)
      
      KH2 <- subset(KH2, select = c(BL,GEN,anzahl_meldebereiche,faelle_covid_aktuell,faelle_covid_aktuell_beatmet,anzahl_standorte,betten_frei,
                                  betten_belegt))
      
      KH2$unbeatmet <- (KH2$faelle_covid_aktuell - KH2$faelle_covid_aktuell_beatmet)
      
      KH2_Beatmung <- subset(KH2, select = c(BL,GEN,faelle_covid_aktuell_beatmet, unbeatmet))
      
      KH2_Beatmung <- KH2_Beatmung %>% 
        rename(
          beatmet = faelle_covid_aktuell_beatmet, 
          unbeatmet = unbeatmet)
      
      KH_Max <- max(KH$faelle_covid_aktuell, na.rm = TRUE)
      KH_Mean <- mean(KH$faelle_covid_aktuell,na.rm = TRUE)
      KH_Breaks <- if(KH_Max  > 19){10} else if(KH_Max> 40){20}else {KH_Max}
      
      KH2_Beatmung <- pivot_longer(KH2_Beatmung,cols = c(beatmet, unbeatmet))
      
    
      Beat<- ggplot(KH2_Beatmung) +
        aes(x = GEN, fill = name, weight = value,text = paste0('Bundesland: ',input$state,'\n','LK/SK: ',GEN,'\n',name,': ',value)) +
        geom_bar() +
        #scale_y_continuous(n.breaks = as.integer(KH_Breaks))+
        scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
        #coord_cartesian(ylim = c(1,KH_Max))+
        scale_fill_brewer(palette = "Paired", direction = 1) +
        labs(x = " ", y = " ", title = "Beatmung der aktuellen Fälle in den Krankenhäusern pro Bundesland", fill = " ") +
        ggthemes::theme_hc()+
        theme(axis.text.x = element_text(size= 9,angle = 90, hjust = 1, vjust = 0.2), axis.text.y = element_text(size = 9)) +
        theme(legend.key.size = unit(0.5,"cm")) +
        theme(legend.position = "top")
      
      ggplotly(Beat, tooltip = 'text')
    })
      output$insgesamt <- renderText({
        KH3 <- KH %>%
          filter(BL %in% input$state)
        
        KH3 <- subset(KH3, select = c(BL,GEN,anzahl_meldebereiche,faelle_covid_aktuell,faelle_covid_aktuell_beatmet,anzahl_standorte,betten_frei,
                                      betten_belegt))
        
        KH3$unbeatmet <- (KH3$faelle_covid_aktuell - KH3$faelle_covid_aktuell_beatmet)
        
        
        KH3_Beatmung <- subset(KH3, select = c(BL,GEN,faelle_covid_aktuell_beatmet, unbeatmet))
        
        KH3_Beatmung <- KH3_Beatmung %>% 
          rename(
            beatmet = faelle_covid_aktuell_beatmet, 
            unbeatmet = unbeatmet)
        
        # KH_Max <- max(KH$faelle_covid_aktuell, na.rm = TRUE)
        # KH_Mean <- mean(KH$faelle_covid_aktuell,na.rm = TRUE)
        # KH_Breaks <- if(KH_Max  > 19){10} else if(KH_Max> 40){20}else {KH_Max}
        
        KH3_Beatmung <- pivot_longer(KH3_Beatmung,cols = c(beatmet, unbeatmet))
        insgesamtv <- sum(as.integer(KH3_Beatmung$value),na.rm = TRUE)
        #p('Insgesamt sind ',style="display:inline",textOutput('insgesamt'),'Menschen im Krankenhaus')
        paste('Insgesamt sind ', insgesamtv, 'Menschen in ',input$state,' im Krankenhaus.')
    })
      output$prozent <- renderText({
        KH3 <- KH %>%
          filter(BL %in% input$state)
        
        KH3 <- subset(KH3, select = c(BL,GEN,anzahl_meldebereiche,faelle_covid_aktuell,faelle_covid_aktuell_beatmet,anzahl_standorte,betten_frei,
                                      betten_belegt))
        #sumfaelle <- sum(KH3$faelle_covid_aktuell)
        #KH3$unbeatmet <- (sum(KH3$faelle_covid_aktuell) / sum(KH3$faelle_covid_aktuell_beatmet))
        prozent <- (sum(KH3$faelle_covid_aktuell_beatmet)*100/sum(KH3$faelle_covid_aktuell))
        prozent <- format(round(prozent,3))
        #p('Es werden',style="display:inline",textOutput('prozent'),'% beatmet'),
        paste('Es werden ',prozent, '% davon beatmet.')
      })
    
    url <- a("Tagesdaten", href="https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0", target="_blank")
    output$Link1 <- renderUI({
      tagList("Link:", url)
    })
    
    url2 <- a("Zeitreihendaten", href="https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0", target="_blank")
    output$Link2 <- renderUI({
      tagList("Link:", url2)
    })
    
    url4 <- a("DVI", href="https://www.intensivregister.de/#/aktuelle-lage/reports", target="_blank")
    output$Link3 <- renderUI({
      tagList("Link:", url4)
    })
    
    url3 <- a("Github", href="https://github.com/Lucyfaria/Shiny-Covid", target="_blank")
    output$GLink <- renderUI({
      tagList("Link:", url3)
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
