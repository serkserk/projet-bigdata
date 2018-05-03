####### Shiny application UI #######

# CREATED BY : Killian Guiheux, Serkan Azap, Corentin Guillo

library(shiny)
library(leaflet)
library(shinythemes)
library(shinydashboard)


ui <- fluidPage(
  shinythemes::themeSelector(),
  
  navbarPage(
    "Trafic Paris",
    
    tabPanel(
      "Acceuil",
      
      navlistPanel(
        tabPanel("Le projet"),
        tabPanel(
          "Les données ",
          
          mainPanel(
            textOutput("text2"),
            fluidRow(valueBoxOutput("countCap"),
                     valueBoxOutput("countTra"))
          ),
          
          
          mainPanel(tabsetPanel(
            tabPanel("Capteur",
                     verbatimTextOutput("acc1")),
            tabPanel("Traffic",
                     verbatimTextOutput("acc2"))
          ))
        ),
        "-----",
        tabPanel("L'équipe",
                 
                 mainPanel(textOutput("text3")))
      )
    ),
    
    tabPanel("Carte des capteurs",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "top",
                   label = strong("Filtre : "),
                   choices = c(
                     "toute les stations" ,
                     "Station avec le plus de traffic",
                     "Station avec le moins de traffic",
                     "heatmap traffic"
                   )
                 ),
                 plotOutput(outputId = "prediction"),
                 plotOutput(
                   outputId = "series",
                   width = "100%",
                   height = "280px"
                 ),
                 plotOutput(
                   outputId = "mois",
                   width = "100%",
                   height = "280px"
                 ),
                 plotOutput(
                   outputId = "jour",
                   width = "100%",
                   height = "280px"
                 )
                 
               ),
               
               mainPanel(
                 titlePanel("Carte des capteurs"),
                 leafletOutput("Carte_capteurs"),
                 dataTableOutput(outputId = "tabTop")
                 
               )
             )),
    tabPanel("Info sur le trafic",
             
             sidebarLayout(
               sidebarPanel(
                 #selectInput(inputId = "choix",
                 #label = strong("Moyenne du débit : "),
                 #            choices = c("par annee" , "par mois", "par jour","par heure"))
                 #),
                 radioButtons(
                   "choix",
                   label = "Moyenne du débit:",
                   choices = c("par annee",
                               "par mois",
                               "par jour",
                               "par heure"),
                   selected = "par annee"
                 )
               ),
               
               mainPanel(plotOutput(outputId = "debitmoyen", width = "100%"))
               
             )),
    
    
    tabPanel("Clustering",
             
             mainPanel(
               plotOutput(outputId = "cluster", width = "100%")
               
             ))
    
  )
)
