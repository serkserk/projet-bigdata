####### Shiny application UI #######

# CREATED BY : Killian Guiheux, Serkan Azap, Corentin Guillo

library(shiny)
library(leaflet)
library(shinythemes)



ui <- fluidPage( theme = shinytheme("superhero"),
  
  tabsetPanel(type = "tabs",
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  selectInput(inputId = "top", label = strong("Station : "),
                              choices = c("toute les stations" , "Station avec le plus de traffic", "Station avec le moins de traffic")),
                  plotOutput(outputId = "series",width = "100%",height = "280px"),
                  plotOutput(outputId = "mois",width = "100%",height = "280px"),
                  plotOutput(outputId = "jour",width = "100%",height = "280px")
                  
                ),
                
                mainPanel(
                  
                  titlePanel("Carte des capteurs"),
                  leafletOutput("Carte_capteurs"),
                  dataTableOutput(outputId = "tabTop")
                  
              )
      )
      
  )
  
)
