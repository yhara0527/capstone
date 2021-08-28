#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)
#library(ggrepel)
#library(gghighlight)
library(stringr)
library(dplyr)
library(sf)
#library(scatterplot3d)
#library(car)
#library(ResourceSelection) # to excute Hosmer-Lemeshow test
#library(equatiomatic)      # to convert model to equation
library(caret)
library(patchwork)         # to put some plots togather
#require(ggiraph)
#require(ggiraphExtra)
#library(plotly)
library(rsconnect)
library(leaflet)
library(mapview)
library(leafsync)
#rsconnect::setAccountInfo(name='yhara', token='7A3D7C23C04C0790CB0A2BCD513893B7', secret='mHKt46XZuYZpkRGquqTandCedRLYb2B3B2GTw1O6')
#deployApp()


# # saveRDS(ica, "ica1")
ica <- readRDS("ica1")

# saveRDS(child_ica_dummy, "cid")
child_ica_dummy <- readRDS("cid")

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    
      # Application title
      titlePanel("School Enrollment Rate by District and Gender"),
  
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
          sidebarPanel(
              
              radioButtons("gender", "Gender:", c("Male"="male","Female"="female")),
              sliderInput("rate_id",
                          "Show School Enrollment Rates Larger Than:",
                          min = 0,
                          max = 1,
                          value = 0.6),
              radioButtons("map_type", "Map Type:", 
                           c("Default"="def",
                             "CartoDB.Positron"="carto",
                             "Esri.NatGeoWorldMap"="esri",
                             "Stamen.Toner"="stamen"))
          ),
  
          # Show a plot of the generated distribution
          mainPanel(
             leafletOutput("map")
          )
      )
  ),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput("prov_id","Select a Province.", unique(child_ica_dummy$Province))
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
    
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
      gen_ind <- 0
          if (input$gender=="female"){
              gen_ind <- 1
          }
          
          co <- ica %>% 
            mutate(centroid = st_centroid(geometry),
                   x = st_coordinates(centroid)[,1],
                   y = st_coordinates(centroid)[,2]) 
          
          temp_data <- child_ica_dummy %>% 
            filter(!is.na(x),Gender==gen_ind) %>%
            group_by(DID) %>%
            summarise(rate=round(mean(School.Enrollment),digits = 2),
                      x=unique(x),
                      y=unique(y),
                      DNAME=unique(DNAME)) %>% 
            filter(rate>=input$rate_id)
          
          pal <- colorNumeric(palette="Reds", domain=temp_data$rate)
          
          switch(input$map_type,
                 "def"=return(leaflet() %>% 
                                addTiles() %>% 
                                setView(lat=mean(co$y), lng=mean(co$x), zoom = 5)%>% 
                                addCircleMarkers(~temp_data$x, ~temp_data$y, label=paste(temp_data$DNAME,temp_data$rate),color=~pal(temp_data$rate), fillOpacity=0.6, radius = 7,stroke=FALSE, data = temp_data) %>% 
                                addLegend(position = "bottomright", pal = pal, values = ~rate,data = temp_data)),
            
                 "carto"=return(leaflet() %>% 
                                addProviderTiles(providers$CartoDB.Positron) %>% 
                                setView(lat=mean(co$y), lng=mean(co$x), zoom = 5)%>% 
                                addCircleMarkers(~temp_data$x, ~temp_data$y, label=paste(temp_data$DNAME,temp_data$rate),color=~pal(temp_data$rate), fillOpacity=0.6, radius = 7,stroke=FALSE, data = temp_data) %>% 
                                addLegend(position = "bottomright", pal = pal, values = ~rate,data = temp_data)),
                 "esri"=return(leaflet() %>% 
                                 addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
                                 setView(lat=mean(co$y), lng=mean(co$x), zoom = 5)%>% 
                                 addCircleMarkers(~temp_data$x, ~temp_data$y, label=paste(temp_data$DNAME,temp_data$rate),color=~pal(temp_data$rate), fillOpacity=0.6, radius = 7,stroke=FALSE, data = temp_data) %>% 
                                 addLegend(position = "bottomright", pal = pal, values = ~rate,data = temp_data)),
                 return(leaflet() %>% 
                          addProviderTiles(providers$Stamen.Toner) %>% 
                          setView(lat=mean(co$y), lng=mean(co$x), zoom = 5)%>% 
                          addPolygons(data = ica,
                                      weight = 1,
                                      label = .$District,
                                      fillOpacity = 0.1) %>%  
                          addCircleMarkers(~temp_data$x, ~temp_data$y, label=paste(temp_data$DNAME,temp_data$rate),color=~pal(temp_data$rate), fillOpacity=0.6, radius = 7,stroke=FALSE, data = temp_data) %>% 
                          addLegend(position = "bottomright", pal = pal, values = ~rate,data = temp_data))
                   
            )
      
        
    })
    output$plot <- renderPlot({
      child_ica_dummy %>% 
        filter(!is.na(x), Province==input$prov_id) %>% 
        group_by(DID,Gender,Age) %>% 
        mutate(rate=mean(School.Enrollment)) %>% 
        ggplot(aes(Age,rate,color=Gender==0)) +
        geom_line() +
        facet_wrap(~DNAME)+
        scale_colour_discrete(labels = c("Female", "Male"))+
        labs(color = "Gender")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
