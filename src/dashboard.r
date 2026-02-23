library(shiny)
library(shinyjs)
library(bslib)
source("implimentVisualization.R")

# Can you add a prettier file on this pls thank you - Joe

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .selectize-dropdown {
      z-index: 99999 !important;
    }
    .js-plotly-plot, .plotly {
      position: relative;
      z-index: 1 !important;
    }
  "))),
  useShinyjs(),
  titlePanel("CG3CG3"),

  sidebarPanel(
    
    fluidRow(
      actionButton("showScatter", "Scatter Plot"),

    ),
    
    fluidRow(
      actionButton("showBar", "Bar Plot"),

    ),
    fluidRow(
      actionButton("showBox", "Box Plot"),

    )
  ),
  mainPanel(
    div(id = "scatterPlot",
      layout_column_wrap(
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
      )),
      plotlyOutput("scatter"),
    ),
    hidden(div(id = "barPlot", 
      plotlyOutput("bar"),
    
    )),
    hidden(div(id= "boxPlot",
      plotlyOutput("box"),
    ))
  )
)

server <- function(input, output, session) {

  # When you add a plot please place it ehre
  plots <- list("scatterPlot", "barPlot", "boxPlot")
  # HANDLES TOGGLE HIDNG PLOTS
  handleShowPlot <- function(plot){
    for(plt in plots){
      hide(plt)
    }

    show(plot)
  }

  # EVENT HANDLES TO HIDE PLOTS
  observeEvent(input$showScatter, {
    handleShowPlot("scatterPlot")
  })

  observeEvent(input$showBar, {
    handleShowPlot("barPlot")
  })

  observeEvent(input$showBox, {
    handleShowPlot("boxPlot")
  })

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