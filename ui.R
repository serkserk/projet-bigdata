####### Shiny application UI #######

# CREATED BY : Killian Guiheux, Serkan Azap, Corentin Guillo

library(shiny)
library(leaflet)



ui <- fluidPage(
  
  tabsetPanel(type = "tabs",
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  plotOutput(outputId = "series",width = "100%",height = "280px")
                  
                ),
                
                mainPanel(
                  
                  titlePanel("Carte des capteurs"),
                  leafletOutput("Carte_capteurs")
                  
              )
      )
      
  )
  
)
