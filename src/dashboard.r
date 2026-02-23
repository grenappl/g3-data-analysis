library(shiny)

source("implimentVisualization.R")

ui <- fluidPage(
  titlePanel("CG3CG3"),
  sidebarPanel(
     selectizeInput( 
        "x_var", 
        "Select X variable below:", 
        choices = list("Study Hours" = "StudyHours", "Attendance" = "Attendance", "Science Score" = "ScienceScore") ,
        selected="StudyHours"
      ), 
      selectizeInput( 
        "y_var", 
        "Select Y variable below:", 
        choices = list("Study Hours" = "StudyHours", "Attendance" = "Attendance", "Science Score" = "ScienceScore") ,
        selected="Attendance"
      ), 
    ),
  mainPanel(
    plotlyOutput("scatter"),
    plotlyOutput("bar"),
    plotlyOutput("box")

  )
)

server <- function(input, output, session) {
  output$scatter <- renderPlotly({
    req(input$x_var, input$y_var)
    scatter_plot(input$x_var, input$y_var)
  })

  output$bar <- renderPlotly({
    bar_plot()
  })

  output$box <- renderPlotly({
    box_plot
  })
}

shinyApp(ui, server)