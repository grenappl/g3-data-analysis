library(shiny)

source("implimentVisualization.R")

ui <- fluidPage(
  titlePanel("EDA Dashboard"),
  plotlyOutput("scatter"),
  plotlyOutput("box")
)

server <- function(input, output, session) {
  output$scatter <- renderPlotly({
    scatter_plot
  })

  output$box <- renderPlotly({
    box_plot
  })
}

shinyApp(ui, server)