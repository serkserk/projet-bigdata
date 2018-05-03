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
                  plotOutput(outputId = "prediction"),
                  plotOutput(outputId = "series",width = "100%",height = "280px"),
                  plotOutput(outputId = "mois",width = "100%",height = "280px"),
                  plotOutput(outputId = "jour",width = "100%",height = "280px")
                  
                ),
                
                mainPanel(
                  
                  fluidRow(
                    valueBoxOutput("countCap"),
                    valueBoxOutput("countTra")),
                  
                  
                  titlePanel("Carte des capteurs"),
                  leafletOutput("Carte_capteurs"),
                  dataTableOutput(outputId = "tabTop")
                  
              )
        )
      ),
      tabPanel("Info sur le trafic",
               
               
            sidebarLayout(
              
              sidebarPanel(
                selectInput(inputId = "choix", label = strong("Moyenne du débit : "),
                            choices = c("par annee" , "par mois", "par jour","par heure"))
              ),
                 
              mainPanel(
                
              plotOutput(outputId = "debitmoyen",width = "100%")
              
              )
              
            )
              
               
               
        )
  )
)
