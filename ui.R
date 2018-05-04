####### Shiny application UI #######

# CREATED BY : Killian Guiheux, Serkan Azap, Corentin Guillo

library(shiny)
library(leaflet)
library(shinythemes)
library(shinydashboard)
library(jsonlite)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage(
    "Trafic Paris",
    
    tabPanel(
      "Acceuil",
      
      navlistPanel(
        tabPanel("Le projet",
                 verbatimTextOutput("introduction")),
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
                 
                 verbatimTextOutput("equipe"))
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
                   ),
                   selected =  "toute les stations"
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
                 ),
                 dataTableOutput("minitable")
               ),
               mainPanel(
                 plotOutput(
                   outputId = "debitmoyen",
                   width = "100%",
                   height = "600px"
                 )
               )
             )),
    
    
    tabPanel(
      "Clustering",
      
      mainPanel(
        titlePanel("Clustering des capteurs"),
        leafletOutput("Clust_capteurs"),
        dataTableOutput(outputId = "tabTop2")
      )
    )
  )
)
