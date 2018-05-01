####### Shiny application UI #######

# CREATED BY : Killian Guiheux, Serkan Azap, Corentin Guillo

library(shiny)
library(leaflet)
library(shinythemes)



ui <- fluidPage( theme = shinytheme("flatly"),
  
  navbarPage( "Menu",
              
              tabPanel("Carte des capteurs",
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  selectInput(inputId = "top", label = strong("Filtre : "),
                              choices = c("toute les stations" , "Station avec le plus de traffic", "Station avec le moins de traffic","heatmap traffic")),
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
      ),
      tabPanel("Info sur le trafic",
                 
               shiny::splitLayout(
                 
                 plotOutput(outputId = "debitparmois",width = "100%"),
                 plotOutput(outputId = "debitparans",width = "99%")
                 

               ),
               plotOutput(outputId = "debitjour")
               
        )
  )
)
