####### Shiny application UI #######

library(shiny)
library(leaflet)



ui <- fluidPage(
  tabsetPanel(type = "tabs",
              mainPanel(
                titlePanel("Titre de la page"),
              plotOutput(outputId = "histogram",width = "100%")
      )
  )
)
