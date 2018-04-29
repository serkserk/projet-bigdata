##################### SHINY SERVER ###########################

######################### LIBRARY ############################

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Projet Big Data"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("My_tab_1", tabName = "tab1", icon = icon("dashboard")),
      menuItem("My_tab_2", tabName = "tab2", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "My_tab_1",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "My_tab_2",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)