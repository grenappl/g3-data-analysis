library(shiny)
library(shinyjs)
library(bslib)
library(leaflet)
source("implimentVisualization.R")
source("geographical_mapping.R")

reflection_file <- readLines("reflection.txt")
reflection <- paste(reflection_file, collapse="\n")

# Can you add a prettier file on this pls thank you - Joe
# erm...

ui <- fluidPage(
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700;800&display=swap');
    @import url('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css');
    
    :root {
      --primary: #2A2F5B;
      --primary-light: #3A4075;
      --primary-dark: #1A1F45;
      --secondary: #00C9A7;
      --secondary-light: #5FF0D1;
      --accent: #FF6B6B;
      --gray-50: #F8FAFC;
      --gray-100: #F1F5F9;
      --gray-200: #E2E8F0;
      --gray-300: #CBD5E1;
      --gray-600: #475569;
      --gray-800: #1E293B;
      --shadow-sm: 0 2px 4px rgba(0,0,0,0.02);
      --shadow-md: 0 4px 6px -1px rgba(0,0,0,0.1);
      --shadow-lg: 0 10px 15px -3px rgba(0,0,0,0.1);
      --shadow-xl: 0 20px 25px -5px rgba(0,0,0,0.1);
    }
    
    * {
      font-family: 'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, sans-serif;
      box-sizing: border-box;
    }
    
    body {
      background: linear-gradient(135deg, #667eea15 0%, #764ba215 100%);
      min-height: 100vh;
      padding: 24px;
    }
    
    .container-fluid {
      background: white;
      border-radius: 32px;
      box-shadow: var(--shadow-xl);
      padding: 32px !important;
      max-width: 1600px;
      margin: 0 auto;
      border: 1px solid var(--gray-200);
    }
    
    .dashboard-header {
      margin-bottom: 32px;
    }
    
    .title-section {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 24px;
    }
    
    .title-wrapper {
      display: flex;
      align-items: center;
      gap: 12px;
    }
    
    .title-icon {
      width: 48px;
      height: 48px;
      background: linear-gradient(135deg, var(--primary) 0%, var(--primary-light) 100%);
      border-radius: 16px;
      display: flex;
      align-items: center;
      justify-content: center;
      color: white;
      font-size: 24px;
      box-shadow: var(--shadow-md);
    }
    
    .title-text {
      font-size: 28px;
      font-weight: 700;
      color: var(--gray-800);
      letter-spacing: -0.5px;
      margin: 0;
    }
    
    .title-text small {
      font-size: 14px;
      font-weight: 400;
      color: var(--gray-600);
      display: block;
      margin-top: 4px;
    }
    
    .stats-grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 20px;
    }
    
    .stat-card {
      background: var(--gray-50);
      border-radius: 20px;
      padding: 20px;
      display: flex;
      align-items: center;
      gap: 16px;
      border: 1px solid var(--gray-200);
      transition: all 0.3s ease;
    }
    
    .stat-card:hover {
      transform: translateY(-2px);
      border-color: var(--secondary);
      box-shadow: var(--shadow-lg);
    }
    
    .stat-icon {
      width: 56px;
      height: 56px;
      background: white;
      border-radius: 16px;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 24px;
      color: var(--primary);
      box-shadow: var(--shadow-sm);
    }
    
    .stat-content h4 {
      font-size: 14px;
      font-weight: 500;
      color: var(--gray-600);
      margin: 0 0 4px 0;
    }
    
    .stat-content .stat-number {
      font-size: 24px;
      font-weight: 700;
      color: var(--gray-800);
      margin: 0;
      line-height: 1.2;
    }
    
    .stat-content .stat-label {
      font-size: 12px;
      color: var(--secondary);
      font-weight: 500;
    }
    
    .well {
      background: white !important;
      border: 1px solid var(--gray-200) !important;
      border-radius: 24px !important;
      box-shadow: var(--shadow-lg) !important;
      padding: 24px !important;
      margin-right: 24px;
    }
    
    .sidebar-header {
      display: flex;
      align-items: center;
      gap: 10px;
      margin-bottom: 20px;
      padding-bottom: 16px;
      border-bottom: 1px solid var(--gray-200);
    }
    
    .sidebar-header i {
      color: var(--primary);
      font-size: 20px;
    }
    
    .sidebar-header h3 {
      font-size: 16px;
      font-weight: 600;
      color: var(--gray-800);
      margin: 0;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    
    .sidebar-header span {
      font-size: 12px;
      color: var(--gray-600);
      margin-left: auto;
    }
    
    .action-button {
      width: 100%;
      padding: 14px 20px;
      margin: 6px 0;
      border: none;
      border-radius: 16px;
      font-weight: 600;
      font-size: 14px;
      text-align: left;
      transition: all 0.3s ease;
      background: white;
      color: var(--gray-600);
      border: 1px solid var(--gray-200);
      display: flex;
      align-items: center;
      gap: 12px;
      position: relative;
      overflow: hidden;
    }
    
    .action-button i {
      width: 20px;
      font-size: 16px;
      color: var(--primary);
      transition: all 0.3s ease;
    }
    
    .action-button:hover {
      background: var(--gray-50);
      border-color: var(--primary);
      transform: translateX(4px);
      color: var(--primary);
    }
    
    .action-button:hover i {
      color: var(--secondary);
    }
    
    .action-button::after {
      content: '';
      position: absolute;
      left: 0;
      top: 0;
      height: 100%;
      width: 3px;
      background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%);
      opacity: 0;
      transition: opacity 0.3s ease;
    }
    
    .action-button:hover::after {
      opacity: 1;
    }
    
    .action-button.active {
      background: linear-gradient(135deg, var(--primary) 0%, var(--primary-light) 100%);
      color: white;
      border: none;
    }
    
    .action-button.active i {
      color: white;
    }
    
    .main-panel {
      background: var(--gray-50);
      border-radius: 24px;
      padding: 24px;
      border: 1px solid var(--gray-200);
    }
    
    .selector-section {
      background: white;
      border-radius: 20px;
      padding: 20px;
      margin-bottom: 24px;
      border: 1px solid var(--gray-200);
      box-shadow: var(--shadow-sm);
    }
    
    .selector-header {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-bottom: 16px;
    }
    
    .selector-header i {
      color: var(--secondary);
      font-size: 18px;
    }
    
    .selector-header span {
      font-size: 13px;
      font-weight: 600;
      color: var(--gray-600);
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    
    .selectize-control {
      margin-bottom: 0;
    }
    
    .selectize-input {
      border: 1px solid var(--gray-200) !important;
      border-radius: 14px !important;
      padding: 12px 16px !important;
      font-size: 14px !important;
      background: white !important;
      box-shadow: var(--shadow-sm) !important;
      transition: all 0.3s ease !important;
    }
    
    .selectize-input.focus {
      border-color: var(--secondary) !important;
      box-shadow: 0 0 0 4px rgba(0, 201, 167, 0.1) !important;
    }
    
    .selectize-dropdown {
      border: none !important;
      border-radius: 16px !important;
      box-shadow: var(--shadow-xl) !important;
      margin-top: 4px !important;
    }
    
    .selectize-dropdown .option {
      padding: 12px 16px !important;
      font-size: 14px !important;
    }
    

    
    .selectize-dropdown .active {
      background: linear-gradient(135deg, var(--primary) 0%, var(--primary-light) 100%) !important;
      color: white !important;
    }
    
    .control-label {
      font-size: 12px;
      font-weight: 600;
      color: var(--gray-600);
      margin-bottom: 6px;
      display: block;
      text-transform: uppercase;
      letter-spacing: 0.3px;
    }
    
    .plot-container {
      background: white;
      border-radius: 20px;
      padding: 20px;
      border: 1px solid var(--gray-200);
      box-shadow: var(--shadow-sm);
      transition: all 0.3s ease;
    }
    
    .plot-container:hover {
      border-color: var(--secondary);
      box-shadow: var(--shadow-lg);
    }
    
    .plot-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 16px;
    }
    
    .plot-title {
      display: flex;
      align-items: center;
      gap: 8px;
    }
    
    .plot-title i {
      color: var(--secondary);
      font-size: 18px;
    }
    
    .plot-title h4 {
      font-size: 14px;
      font-weight: 600;
      color: var(--gray-800);
      margin: 0;
    }
    
    .plot-badge {
      padding: 4px 12px;
      background: var(--gray-100);
      border-radius: 100px;
      font-size: 11px;
      font-weight: 500;
      color: var(--gray-600);
      display: flex;
      align-items: center;
      gap: 6px;
    }
    
    .plot-badge i {
      color: var(--secondary);
      font-size: 12px;
    }
    
    .leaflet-container {
      border-radius: 16px;
      border: 1px solid var(--gray-200);
    }
    
    .fade-enter {
      animation: slideIn 0.4s cubic-bezier(0.16, 1, 0.3, 1);
      margin-bottom: 24px;
    }
    
    @keyframes slideIn {
      from {
        opacity: 0;
        transform: translateY(10px);
      }
      to {
        opacity: 1;
        transform: translateY(0);
      }
    }
    
    .shiny-notification {
      background: white !important;
      border: none !important;
      border-radius: 16px !important;
      box-shadow: var(--shadow-xl) !important;
      padding: 16px !important;
      font-family: 'Plus Jakarta Sans', sans-serif !important;
      border-left: 4px solid var(--secondary) !important;
    }
    
    ::-webkit-scrollbar {
      width: 8px;
    }
    
    ::-webkit-scrollbar-track {
      background: var(--gray-100);
    }
    
    ::-webkit-scrollbar-thumb {
      background: var(--primary);
      border-radius: 4px;
    }
    
    ::-webkit-scrollbar-thumb:hover {
      background: var(--secondary);
    }
    
    @media (max-width: 992px) {
      .stats-grid {
        grid-template-columns: repeat(2, 1fr);
      }
    }
    
    @media (max-width: 768px) {
      body {
        padding: 12px;
      }
      
      .container-fluid {
        padding: 16px !important;
      }
      
      .well {
        margin-right: 0;
        margin-bottom: 20px;
      }
      
      .stats-grid {
        grid-template-columns: 1fr;
      }
      
      .title-section {
        flex-direction: column;
        align-items: flex-start;
        gap: 16px;
      }
    }
    
    .shiny-output-error {
      color: var(--accent);
      background: #FEF2F2;
      border-radius: 12px;
      padding: 12px;
      border-left: 3px solid var(--accent);
    }
    
    .tooltip-inner {
      background: var(--primary);
      border-radius: 8px;
      padding: 8px 12px;
      font-size: 12px;
    }
  "))),
  
  useShinyjs(),
  
    div(class = "dashboard-header",
        div(class = "title-section",
            div(class = "title-wrapper",
                div(class = "title-icon",
                    HTML("<i class='fas fa-chart-pie'></i>")
                ),
                div(class = "title-text",
                    "CG3CG3 Analytics",
                    HTML("<small>Data Visualization Dashboard</small>")
                )
            ),
            div(class = "date-badge",
                HTML("<i class='far fa-calendar-alt'></i> ", format(Sys.Date(), "%B %d, %Y"))
            )
        ),
        
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-icon",
                      HTML("<i class='fas fa-chart-line'></i>")
                  ),
                  div(class = "stat-content",
                      h4("Total Visualizations"),
                      p(class = "stat-number", "5"),
                      span(class = "stat-label", "↑ 2 new this week")
                  )
              ),
              div(class = "stat-card",
                  div(class = "stat-icon",
                      HTML("<i class='fas fa-database'></i>")
                  ),
                  div(class = "stat-content",
                      h4("Data Points"),
                      p(class = "stat-number", "1,284"),
                      span(class = "stat-label", "3 variables available")
                  )
              ),
              div(class = "stat-card",
                  div(class = "stat-icon",
                      HTML("<i class='fas fa-clock'></i>")
                  ),
                  div(class = "stat-content",
                      h4("Last Update"),
                      p(class = "stat-number", "Now"),
                      span(class = "stat-label", "Real-time data")
                  )
              )
          )
    ),
  
    div(class = "sidebar-wrapper",
        sidebarPanel(
          div(class = "sidebar-header",
              HTML("<i class='fas fa-sliders-h'></i>"),
              h3("Visualization Controls"),
              span("v2.0")
          ),
          fluidRow(
            actionButton("showScatter", 
                         HTML("<i class='fas fa-chart-scatter'></i> Scatter Plot"),
                         class = "action-button"
            )
          ),
          fluidRow(
            actionButton("showBar", 
                         HTML("<i class='fas fa-chart-bar'></i> Bar Plot"),
                         class = "action-button"
            )
          ),
          fluidRow(
            actionButton("showBox", 
                         HTML("<i class='fas fa-chart-box'></i> Box Plot"),
                         class = "action-button"
            )
          ),
          fluidRow(
            actionButton("showHist", 
                         HTML("<i class='fas fa-chart-box'></i> Histogram"),
                         class = "action-button"
            )
          ),
          fluidRow(
            actionButton("showGeo", 
                         HTML("<i class='fas fa-map-marked-alt'></i> Geographical Mapping"),
                         class = "action-button"
            )
          ),
          br(),
          div(class = "sidebar-footer",
              HTML("<i class='fas fa-info-circle'></i> Click any button to switch visualization")
          )
        )
    ),
  
    div(class = "main-panel-wrapper",
        mainPanel(
          div(id = "scatterPlot", class = "fade-enter",
              div(class = "selector-section",
                  div(class = "selector-header",
                      HTML("<i class='fas fa-sliders-h'></i>"),
                      span("Variable Selection")
                  ),
                  layout_column_wrap(
                    selectizeInput( 
                      "x_var", 
                      "X-Axis Variable", 
                      choices = list("Study Hours" = "StudyHours", "Attendance" = "Attendance", "Science Score" = "ScienceScore"),
                      selected = "StudyHours"
                    ), 
                    selectizeInput( 
                      "y_var", 
                      "Y-Axis Variable", 
                      choices = list("Study Hours" = "StudyHours", "Attendance" = "Attendance", "Science Score" = "ScienceScore"),
                      selected = "Attendance"
                    )
                  )
              ),
              div(class = "plot-container",
                  div(class = "plot-header",
                      div(class = "plot-title",
                          HTML("<i class='fas fa-chart-scatter'></i>"),
                          h4("Scatter Plot Analysis")
                      ),
                      div(class = "plot-badge",
                          HTML("<i class='fas fa-sync-alt'></i> Live Update")
                      )
                  ),
                  plotlyOutput("scatter")
              )
          ),
          hidden(div(id = "barPlot", class = "fade-enter",
                     div(class = "plot-container",
                         div(class = "plot-header",
                             div(class = "plot-title",
                                 HTML("<i class='fas fa-chart-bar'></i>"),
                                 h4("Bar Chart Distribution")
                             ),
                             div(class = "plot-badge",
                                 HTML("<i class='fas fa-chart-simple'></i> Categorical Data")
                             )
                         ),
                         plotlyOutput("bar")
                     )
          )),
          hidden(div(id = "boxPlot", class = "fade-enter",
                     div(class = "plot-container",
                         div(class = "plot-header",
                             div(class = "plot-title",
                                 HTML("<i class='fas fa-chart-box'></i>"),
                                 h4("Box Plot Statistics")
                             ),
                             div(class = "plot-badge",
                                 HTML("<i class='fas fa-calculator'></i> Summary Stats")
                             )
                         ),
                         plotlyOutput("box")
                     )
          )),

          hidden(div(id = "histogramPlot", class = "fade-enter",
                    div(class = "selector-section",
                      div(class = "selector-header",
                          HTML("<i class='fas fa-sliders-h'></i>"),
                          span("Variable Selection")
                      ),
                      layout_column_wrap(
                        selectizeInput( 
                          "hist", 
                          "X-Axis Variable", 
                          choices = list("Study Hours" = "StudyHours", "Attendance" = "Attendance", "Science Score" = "ScienceScore"),
                          selected = "StudyHours"
                        ),
                        sliderInput("histBins", "Bins", 
                          min = 10, max = 25, value = 10), 
                      )
                    ),
                    div(class = "plot-container",
                        div(class = "plot-header",
                            div(class = "plot-title",
                                HTML("<i class='fas fa-chart-histogram'></i>"),
                                h4("Histogram")
                            ),
                            div(class = "plot-badge",
                                HTML("<i class='fas fa-sync-alt'></i> Data distribution")
                            )
                        ),
                        plotlyOutput("histogram",  height = "600px")
              )
          )),
          hidden(div(id = "geoPlot", class = "fade-enter",
                     div(class = "plot-container",
                         div(class = "plot-header",
                             div(class = "plot-title",
                                 HTML("<i class='fas fa-map-marked-alt'></i>"),
                                 h4("Geographical Distribution")
                             ),
                             div(class = "plot-badge",
                                 HTML("<i class='fas fa-globe'></i> World Map")
                             )
                         ),
                         leafletOutput("geo", height = "400px")
                     )
          ))
        )
    ),
  
    div(class="stat-card",
      div(class = "stat-content",
          div(style = "
                display: flex;
                flex-direction: row; 
                align-items: center;",
              div(class = "stat-icon",
                  HTML("<i class='far fa-lightbulb'></i>")
              ),
              h3(style="font-weight: bold; margin-bottom: 20px;", "Reflection"),
          ),
          p(style="text-align: justify; margin: 16px;", reflection)
      )
    )
)

server <- function(input, output, session) {
  # When you add a plot please place it ehre
  plots <- list("scatterPlot", "barPlot", "boxPlot", "geoPlot", "histogramPlot")
  
  # EVENT HANDLES TO HIDE PLOTS
  observeEvent(input$showScatter, {
    handleShowPlot("scatterPlot")
    updateButtonStates("showScatter")
  })
  
  observeEvent(input$showBar, {
    handleShowPlot("barPlot")
    updateButtonStates("showBar")
  })

  observeEvent(input$showHist, {
    handleShowPlot("histogramPlot")
    updateButtonStates("showHist")
  })
  
  observeEvent(input$showBox, {
    handleShowPlot("boxPlot")
    updateButtonStates("showBox")
  })
  
  observeEvent(input$showGeo, {
    handleShowPlot("geoPlot")
    updateButtonStates("showGeo")
  })
  
  handleShowPlot <- function(plot){
    for(plt in plots){
      hide(plt)
    }
    show(plot)
  }
  
  # Update button active states
  updateButtonStates <- function(active) {
    # This would require JavaScript for full implementation
    # CSS handles the visual states
  }
  
  output$scatter <- renderPlotly({
    req(input$x_var, input$y_var)
    scatter_plot(input$x_var, input$y_var)
  })
  
  output$bar <- renderPlotly({
    bar_plot()
  })


  output$histogram <- renderPlotly({
    req(input$hist)
    plot_histogram(input$hist, input$histBins)
  })
  
  outputOptions(output, "histogram", suspendWhenHidden = TRUE)

  output$box <- renderPlotly({
    box_plot()
  })
  
  output$geo <- renderLeaflet({
    geographical_plot()
  }) 
}

shinyApp(ui, server)