library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(rpart)
library(ggplot2)

source('data_visuals.r')
source('knn_model.r')
source('group_name.r')
source('data_visuals.r')

css <- "
  @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,500;0,600;0,700;0,800;1,600&family=Source+Sans+3:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&display=swap');

  /* ── 0. Cinematic Loading Screen (Themed) ── */
  .loader-overlay {
    position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;
    background: linear-gradient(180deg, #1E1A17 0%, #0F0D0B 100%); 
    z-index: 999999; display: flex; flex-direction: column; justify-content: center; align-items: center;
    animation: removeOverlay 4s cubic-bezier(0.8, 0, 0.2, 1) forwards;
  }
  .loader-scene {
    position: relative; width: 300px; height: 120px;
    border-bottom: 2px solid rgba(217, 206, 195, 0.2); overflow: hidden;
  }
  .loader-water {
    position: absolute; bottom: -5px; left: 0; width: 100%; height: 10px;
    background: #344C65; 
    border-radius: 50%; box-shadow: 0 0 20px #344C65;
  }
  .loader-iceberg {
    position: absolute; bottom: 0; right: 40px;
    width: 0; height: 0;
    border-left: 30px solid transparent; border-right: 30px solid transparent;
    border-bottom: 60px solid #D9CEC3; 
    filter: drop-shadow(0 0 10px rgba(217, 206, 195, 0.15));
  }
  .loader-ship {
    position: absolute; bottom: 0; left: -50px;
    font-size: 45px; color: #E9BA24; 
    animation: sailAndHit 3s ease-in-out forwards;
  }
  .loader-text {
    font-family: 'Playfair Display', serif; color: #AC6C35; 
    font-size: 24px; font-weight: 700; margin-top: 30px; letter-spacing: 2px;
    text-transform: uppercase;
    animation: pulseText 1.5s infinite alternate;
  }
  
  @keyframes sailAndHit {
    0%   { transform: translateX(0) rotate(0); }
    45%  { transform: translateX(165px) rotate(0); } 
    50%  { transform: translateX(175px) rotate(-5deg); } 
    65%  { transform: translateX(175px) translateY(10px) rotate(-15deg); } 
    100% { transform: translateX(175px) translateY(80px) rotate(-35deg); opacity: 0;} 
  }
  @keyframes removeOverlay {
    0%, 80% { opacity: 1; visibility: visible; z-index: 999999;}
    99% { opacity: 0; visibility: hidden; z-index: 999999;}
    100% { opacity: 0; visibility: hidden; z-index: -1; display: none; }
  }
  @keyframes pulseText {
    0% { opacity: 0.5; text-shadow: 0 0 5px transparent; }
    100% { opacity: 1; text-shadow: 0 0 15px rgba(233,186,36,0.3); color: #E9BA24;}
  }

  /* ── 1. Base & Background ── */
  *, *::before, *::after { box-sizing: border-box; }
  body, .content-wrapper, .right-side {
    background: #D9CEC3 !important; 
    font-family: 'Source Sans 3', sans-serif;
    color: #2A2521 !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }

  /* ── 2. Maximize Space (Tighter Grid) ── */
  .content { padding: 15px 20px !important; }
  .row { margin-left: -8px !important; margin-right: -8px !important; }
  .col-sm-1, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9, .col-sm-10, .col-sm-11, .col-sm-12 {
    padding-left: 8px !important; padding-right: 8px !important;
  }

  /* ── 3. Animations ── */
  @keyframes slideInUp {
    0% { opacity: 0; transform: translateY(15px); }
    100% { opacity: 1; transform: translateY(0); }
  }
  @keyframes slideInRight {
    0% { opacity: 0; transform: translateX(-15px); }
    100% { opacity: 1; transform: translateX(0); }
  }

  /* ── 4. Improved Sidebar ── */
  .main-sidebar, .left-side { 
    background: linear-gradient(180deg, #1E1A17 0%, #0F0D0B 100%) !important; 
    border-right: 1px solid #3A322C !important; box-shadow: 4px 0 20px rgba(0,0,0,0.25); z-index: 100;
  }
  .main-header .logo { 
    width: 230px; background: #110F0D !important; 
    font-family:'Playfair Display',serif; font-size:18px; font-weight: 800; color:#E9BA24 !important; 
    letter-spacing: 1.5px; text-transform: uppercase; box-shadow: 0 2px 10px rgba(0,0,0,0.5);
  }
  .main-header .navbar { 
    background: #2A2521 !important; box-shadow: 0 2px 10px rgba(0,0,0,0.15); border-bottom: none !important;
  }
  .main-header .sidebar-toggle { color: #E9BA24 !important; transition: all 0.3s ease;}
  .main-header .sidebar-toggle:hover { background: rgba(233,186,36,0.1) !important; transform: scale(1.1); }
  
  .sidebar-menu .header { 
    color: #6C5F54 !important; font-size:11px; font-weight:800; letter-spacing:2px; text-transform:uppercase; padding:25px 20px 10px !important; 
  }
  .sidebar-menu > li { animation: slideInRight 0.4s ease-out both; }
  .sidebar-menu > li:nth-child(1) { animation-delay: 0.1s; }
  .sidebar-menu > li:nth-child(2) { animation-delay: 0.2s; }
  .sidebar-menu > li:nth-child(3) { animation-delay: 0.3s; }
  .sidebar-menu > li:nth-child(4) { animation-delay: 0.4s; }
  .sidebar-menu > li:nth-child(5) { animation-delay: 0.5s; }

  .sidebar-menu > li > a { 
    color: #A99A8C !important; font-size:14px; font-weight:600; padding:14px 20px !important; 
    border-left: 4px solid transparent; transition: all 0.3s ease; 
  }
  .sidebar-menu > li > a .fa { width:28px; transition: transform 0.3s ease; font-size: 16px; }
  .sidebar-menu > li > a:hover, .sidebar-menu > li.active > a { 
    color: #E9BA24 !important; background: linear-gradient(90deg, rgba(233,186,36,0.1) 0%, transparent 100%) !important; 
    border-left-color: #E9BA24 !important; padding-left: 26px !important;
  }
  .sidebar-menu > li:hover > a .fa { transform: translateX(4px) scale(1.1); color: #E9BA24; }

  /* ── 5. Gorgeous Header Titles ── */
  .page-title, h2 { 
    font-family:'Playfair Display',serif; font-size:30px; font-weight:800; color:#1E1A17; margin:5px 0 20px; 
    letter-spacing: 0.5px; text-transform: uppercase; border-left: 6px solid #AC6C35; padding-left: 14px;
    animation: slideInRight 0.4s ease-out both;
  }
  .divider { display: none; } 

  /* ── 6. Standout Analytics Boxes ── */
  .box { 
    border-radius:10px !important; box-shadow: 0 4px 12px rgba(42,37,33,0.06), 0 1px 3px rgba(0,0,0,0.04) !important; 
    border: none !important; border-top: 4px solid #344C65 !important; 
    background: linear-gradient(145deg, #FDFBF8 0%, #EFEBE5 100%) !important; 
    transition: all 0.3s ease; animation: slideInUp 0.4s ease-out both; margin-bottom: 16px !important; 
  }
  .box:hover { 
    transform: translateY(-3px); box-shadow: 0 12px 24px rgba(42,37,33,0.12), 0 4px 8px rgba(0,0,0,0.06) !important; 
    border-top: 4px solid #AC6C35 !important; 
  }
  .box-header { background:transparent !important; border-bottom:1px solid rgba(42,37,33,0.08) !important; padding:12px 16px 8px !important; }
  .box-title  { font-family:'Playfair Display',serif !important; font-size:16px !important; font-weight:800 !important; color:#2A2521 !important; letter-spacing: 0.5px; text-transform: uppercase;}
  .box-body   { padding:12px 16px 16px !important; } 

  /* ── 7. KPI Tiles ── */
  .kpi-row { display:flex; gap:16px; flex-wrap:wrap; margin-bottom:20px; }
  .kpi-tile { 
    flex:1; min-width:180px; border-radius:10px; padding:20px 24px; color:#fff; 
    box-shadow: 0 6px 15px rgba(0,0,0,0.1); position: relative; overflow: hidden; 
    transition: all 0.3s ease; animation: slideInUp 0.4s ease-out both;
  }
  .kpi-tile:hover { transform: translateY(-5px); box-shadow: 0 12px 25px rgba(0,0,0,0.15); }
  .kpi-tile:nth-child(1) { animation-delay: 0.1s; }
  .kpi-tile:nth-child(2) { animation-delay: 0.2s; }
  .kpi-tile:nth-child(3) { animation-delay: 0.3s; }
  .kpi-tile:nth-child(4) { animation-delay: 0.4s; }
  
  .kpi-val  { font-family:'DM Mono',monospace; font-size:38px; font-weight:500; z-index: 2; position: relative;}
  .kpi-lbl  { font-size:12px; font-weight:700; letter-spacing:1px; text-transform:uppercase; opacity:0.9; margin-top:4px; z-index: 2; position: relative;}
  .kpi-icon { font-size:60px; opacity:0.12; position: absolute; right: 10px; bottom: 0px; z-index: 1; transition: all 0.4s ease;}
  .kpi-tile:hover .kpi-icon { transform: scale(1.2) rotate(-10deg); opacity: 0.2; }
  
  .kpi-dark  { background: linear-gradient(135deg, #3E3630 0%, #1A1714 100%); }
  .kpi-navy  { background: linear-gradient(135deg, #4A6887 0%, #233446 100%); }
  .kpi-orange  { background: linear-gradient(135deg, #AC6C35 0%, #976132 100%); }
  .kpi-gold  { background: linear-gradient(135deg, #C5A059 0%, #967410 100%); }

  /* ── 8. Beautiful Table Columns for Dataset ── */
  .dataTables_wrapper { 
    background: #FFF; padding: 15px; border-radius: 10px; box-shadow: 0 5px 15px rgba(0,0,0,0.05); animation: slideInUp 0.4s ease-out both; 
  }
  table.dataTable { border-collapse: collapse !important; width: 100% !important; margin-top: 10px !important;}
  table.dataTable thead th { 
    background: #233446 !important; color: #E9BA24 !important; border: none !important; border-right: 1px solid rgba(255,255,255,0.15) !important; font-size: 13px; font-weight: 700; text-transform: uppercase; letter-spacing: 1px; padding: 16px !important; 
  }
  table.dataTable thead th:last-child { border-right: none !important; }
  table.dataTable tbody td { 
    background: #FFFFFF !important; border-bottom: 1px solid #EBE3DB !important; border-right: 1px solid #F0EAE3 !important; padding: 14px 16px !important; font-size: 14px; font-weight: 500;
  }
  table.dataTable tbody td:last-child { border-right: none !important; }
  table.dataTable tbody tr:nth-child(even) td { background: #FAF7F2 !important; } 
  table.dataTable tbody tr { transition: all 0.2s; }
  table.dataTable tbody tr:hover td { background: #EAE0D5 !important; color: #1E1A17 !important;}
  .dataTables_info, .dataTables_length label, .dataTables_filter label { color:#5A5047 !important; font-size:13px; font-weight: 700; }
  .dataTables_filter input, .dataTables_length select { background:#FFF !important; border:2px solid #DBCBBD !important; border-radius:6px !important; padding: 4px 10px;}
  .dataTables_paginate .paginate_button { color:#2A2521 !important; border-radius: 6px !important; border: none !important; font-weight: 600;}
  .dataTables_paginate .paginate_button.current { background: #344C65 !important; color:#fff !important; box-shadow: 0 4px 8px rgba(52,76,101,0.3) !important;}

  /* ── 9. PREDICTIONS PAGE LAYOUT ── */
  #shiny-tab-predictions .row { display: flex; flex-wrap: wrap; margin-top: 10px; }
  #shiny-tab-predictions .col-sm-8 {
    background: #FDFBF8; border-radius: 12px; padding: 30px; box-shadow: 0 8px 25px rgba(42,37,33,0.06); border-top: 5px solid #344C65; border-bottom: 5px solid #AC6C35; animation: slideInUp 0.4s ease-out both; 
  }
  #shiny-tab-predictions .well {
    background: #233446 !important; border: none !important; border-radius: 12px !important; box-shadow: 0 10px 25px rgba(0,0,0,0.15) !important; color: #FFFFFF !important; padding: 30px 25px !important; animation: slideInUp 0.4s ease-out both; 
  }
  #shiny-tab-predictions .well label {
    color: #DBCBBD !important; font-size: 12px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase; margin-bottom: 8px;
  }
  #shiny-tab-predictions .well .form-control, #shiny-tab-predictions .well select {
    background: rgba(255,255,255,0.05) !important; border: 1px solid rgba(255,255,255,0.15) !important; color: #FFF !important; border-radius: 8px !important; padding: 10px 12px; height: auto;
  }
  #shiny-tab-predictions .well .form-control:focus, #shiny-tab-predictions .well select:focus {
    border-color: #E9BA24 !important; box-shadow: 0 0 0 3px rgba(233,186,36,0.2) !important; outline: none;
  }
  #shiny-tab-predictions .well select option { background: #233446; color: #FFF; }
  
  #shiny-tab-predictions .btn-primary {
    background: linear-gradient(135deg, #C5A059 0%, #967410 100%) !important; color: #FFF !important; border: none !important; border-radius: 8px !important; padding: 14px 20px !important; font-size: 16px !important; font-weight: 800 !important; width: 100%; margin-top: 20px; text-transform: uppercase; letter-spacing: 1.5px; box-shadow: 0 6px 15px rgba(197, 160, 89, 0.4) !important; transition: all 0.3s ease;
  }
  #shiny-tab-predictions .btn-primary:hover {
    transform: translateY(-2px); box-shadow: 0 10px 20px rgba(197, 160, 89, 0.6) !important;
  }
  #shiny-tab-predictions pre#prediction {
    background: #FFFFFF !important; border: none !important; border-radius: 10px !important; color: #2A2521 !important; font-family: 'Playfair Display', serif !important; font-size: 32px !important; font-weight: 800 !important; padding: 25px !important; text-align: center !important; box-shadow: 0 4px 15px rgba(0,0,0,0.05) !important; text-transform: uppercase; letter-spacing: 2px; border-left: 8px solid #E9BA24 !important; margin: 20px 0 30px 0;
  }
  #shiny-tab-predictions h3 { font-family: 'Playfair Display', serif; font-weight: 800; color: #233446; font-size: 24px; margin-top: 0; text-transform: uppercase; letter-spacing: 1px;}
  #shiny-tab-predictions h4 { font-family: 'Playfair Display', serif; font-weight: 700; color: #AC6C35; border-bottom: 2px solid #EBE3DB; padding-bottom: 10px; margin-top: 40px; margin-bottom: 25px;}

  #shiny-tab-predictions .irs--shiny .irs-bar { border: none; background: #E9BA24; }
  #shiny-tab-predictions .irs--shiny .irs-single { background-color: #E9BA24; color: #233446; font-weight: 700; border-radius: 4px;}
  #shiny-tab-predictions .irs--shiny .irs-handle { border: 3px solid #E9BA24; background: #233446; }
  #shiny-tab-predictions .irs--shiny .irs-min, #shiny-tab-predictions .irs--shiny .irs-max { background: transparent; color: rgba(255,255,255,0.5); }


  /* ── 10. Team Cards (Large & Spacious) ── */
  /* Increased minmax from 200px to 300px for larger boxes */
  .team-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(300px,1fr)); gap:30px; padding-bottom: 30px;}
  
  .team-card { 
    background: linear-gradient(145deg, #FDFBF8, #EFEBE5); border-radius:16px; 
    padding:40px 30px; /* Increased padding */
    text-align:center; 
    box-shadow:0 8px 20px rgba(42,37,33,0.06); border-top: 5px solid #AC6C35; 
    transition: all 0.3s ease; animation: slideInUp 0.4s ease-out both; 
  }
  
  .team-card:hover { transform: translateY(-10px); box-shadow:0 15px 35px rgba(42,37,33,0.12); border-top-color: #344C65;}
  
  .team-avatar { 
    width:100px; height:100px; /* Bigger avatar */
    border-radius:50%; margin:0 auto 20px; display:flex; align-items:center; justify-content:center; 
    font-family:'Playfair Display',serif; font-size:36px; font-weight:800; color:#fff; 
    box-shadow: 0 8px 20px rgba(0,0,0,0.15); transition: transform 0.3s ease;
  }
  .team-card:hover .team-avatar { transform: scale(1.1); }
  
  .team-name { font-family:'Playfair Display',serif; font-size:24px; font-weight:800; color:#2A2521; }
  .team-role { font-size:13px; color:#AC6C35; font-weight:800; margin-top:8px; text-transform:uppercase; letter-spacing:1.5px; }
  
  /* Full text shown neatly - no truncation */
  .team-card .team-bio  { 
    font-size:15px; color:#6B5F58; margin-top:18px; line-height:1.7;
  }

  #prediction {
    font-size: 24px;
    font-weight: bold;
    text-align: center;
  }

  /* ── 11. Custom Scrollbars ── */
  ::-webkit-scrollbar { width:8px; height:8px; }
  ::-webkit-scrollbar-track { background: #D9CEC3; }
  ::-webkit-scrollbar-thumb { background: #AC6C35; border-radius:4px; }
  ::-webkit-scrollbar-thumb:hover { background: #8C5628; }
"

member <- function(initials, m) {
  div(class = "team-card",
      div(class = "team-avatar", style = paste0("background:", m$color, ";"), initials),
      div(class = "team-name", m$name),
      div(class = "team-role", m$role),
      div(class = "team-bio",  m$bio)
  )
}

# UI
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Titanic Dashboard"
  ),
  
  dashboardSidebar(
    tags$style(HTML(css)),
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview",    tabName = "overview",    icon = icon("ship")),
      menuItem("Analytics",   tabName = "analytics",   icon = icon("chart-bar")),
      menuItem("Predictions", tabName = "predictions", icon = icon("brain")),
      menuItem("Dataset",     tabName = "dataset",     icon = icon("table")),
      menuItem("Team",        tabName = "team",        icon = icon("users"))
    )
  ),
  
  dashboardBody(
    
    # Cinematic Loading Screen
    tags$div(class = "loader-overlay",
             tags$div(class = "loader-scene",
                      tags$div(class = "loader-iceberg"),
                      tags$div(class = "loader-ship", icon("ship")),
                      tags$div(class = "loader-water")
             ),
             tags$div(class = "loader-text", "Loading Titanic Dashboard...")
    ),
    
    tabItems(
      
      # OVERVIEW
      tabItem(tabName = "overview",
              tags$h2("Overview", class = "page-title"),
              tags$hr(class = "divider"),
              
              div(class = "kpi-row",
                  kpi(nrow(titanic), "Total Passengers", "users", "kpi-dark"),
                  kpi(paste0(round(mean(titanic$Survived == 1) * 100,1),"%"), "Survival Rate", "life-ring", "kpi-navy"),
                  kpi(paste0("£", round(mean(titanic$Fare), 2)), "Mean Fare", "sterling-sign", "kpi-gold")
              ),
              
              fluidRow(
                box(title = "Survival by Passenger Class", width = 6,
                    plotlyOutput("ov_class", height = "250px")
                ),
                box(title = "Survival by Sex", width = 6,
                    plotlyOutput("ov_sex", height = "250px")
                )
              ),
              fluidRow(
                box(title = "Survival by Age", width = 7,
                    plotlyOutput("ov_age", height = "270px")
                ),
                box(title = "Passenger Class Proportion", width = 5,
                    plotlyOutput("an_pie", height = "270px")
                )
              )
      ),
      
      # ANALYTICS
      tabItem(tabName = "analytics",
              tags$h2("Analytics", class = "page-title"),
              tags$hr(class = "divider"),
              
              fluidRow(
                box(title = "Age vs Fare - Colored by Survival", width = 12,
                    plotlyOutput("an_scatter", height = "460px")
                )
              ),
              fluidRow(
                box(title = "Fare Distribution by Passenger Class", width = 12,
                    plotlyOutput("an_fare", height = "460px")
                )
              ),
              fluidRow(
                box(title = "Fare Distribution by Survival", width = 7,
                    plotlyOutput("an_box", height = "460px")
                ),
                box(title = "Survival Rate by Class and Sex", width = 5,
                    plotlyOutput("an_heatmap", height = "460px")
                )
              )
      ),
      
      # PREDICTIONS
      tabItem(tabName = "predictions",
              tags$h2("Knn Predictions", class = "page-title"),
              tags$hr(class = "divider"),

              fluidRow(
                column(width = 5,
                  box(title = "Passenger Data Input", width = 12,
                    div(style = "padding: 15px;",
                      sliderInput("kneighbors", "# of Neighbours:",
                                  min = 1, max = 150, value = 3),
                      selectInput("pclass", "Passenger Class", choices = c(1, 2, 3)),
                      selectInput("sex", "Sex", choices = c("male", "female")),
                      numericInput("age", "Age", value = 30, min = 1, max = 100),
                      numericInput("fare", "Fare", value = 32, min = 0, max = 600),
                      actionButton("predict_btn", "Predict", class = "btn-primary"),
                    )
                  ),
                  box(width = 12,
                    textOutput("prediction")
                  )
                ),
                column(width = 7,
                  h3("Model Quality"),
                  htmlOutput("model_qual"),
                  br(),
                  h3("Confusion Matrix Values"),
                  htmlOutput("conf_mat")
                )
              ),
              
              fluidRow(
                column(width=12,
                  br(),
                  box(title = "PCA decision boundary – PC1 vs PC2", width = 12,
                    plotlyOutput("pca_boundary")
                  ),
                  br(),
                  box(title = "K vs Accuracy plot", width = 12,
                    plotlyOutput("accuracy_k_plot")
                  ),
                  br(),
                  box(title = "K comparisons", width = 12,
                    plotlyOutput("differentK")
                  )
                )
              )
      ),
      
      # DATASET
      tabItem(tabName = "dataset",
              tags$h2("Dataset", class = "page-title"),
              tags$hr(class = "divider"),
              
              fluidRow(
                box(width = 12,
                    DTOutput("main_table")
                )
              )
      ),
      
      # TEAM
      tabItem(tabName = "team",
              tags$h2("Team", class = "page-title"),
              tags$hr(class = "divider"),
              
              div(class = "team-grid",
                  member("JP", team[[1]]),
                  member("IF", team[[2]]),
                  member("DN", team[[3]]),
                  member("IR", team[[4]]),
                  member("AS", team[[5]])
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  
  
  # Overview
  output$ov_class <- renderPlotly({
    ov_class()
  })
  
  output$ov_sex <- renderPlotly({
    ov_sex()
  })
  
  output$ov_age <- renderPlotly({
    ov_age()
  })
  
  output$an_pie <- renderPlotly({
    an_pie()
  })
  
  # Analytics
  output$an_fare <- renderPlotly({
    an_fare()
  })
  
  output$an_heatmap <- renderPlotly({
    an_heatmap()
  })
  
  output$an_scatter <- renderPlotly({
    an_scatter()
  })
  
  output$an_box <- renderPlotly({
    an_box()
  })
  
  ## ANAKIN STUFF
  
  output$an_age_hist <- renderPlotly({
    an_age_hist() 
  })
  output$an_embark   <- renderPlotly({ 
    an_embark() 
  })
  
  output$an_family   <- renderPlotly({
    an_family() 
  })

  output$an_violin   <- renderPlotly({ 
    an_violin()
  })
  ##
  
  ## JOE STUFF
  predicted_point <- reactiveVal(NULL)
  
  knn_results <- reactive({
    test_scaled_full <- predict(preproc, test[, numeric_cols])
    
    pred_all <- knn(train = train_scaled,
                    test  = test_scaled_full,
                    cl    = train$Survived,
                    k     = input$kneighbors)   
    
    confusionMatrix(pred_all, test$Survived)
  })
  
  # Use it in outputs — updates whenever slider moves
  output$model_qual <- renderUI({
    model_qual(knn_results())
  })

  output$conf_mat <- renderUI({
    conf_mat(knn_results())
  })
  
  output$pca_boundary <- renderPlotly({PCA_boundary(input$kneighbors, predicted_point())})
  
  output$accuracy_k_plot <- renderPlotly({
    K_accuracy_plot(input$kneighbors)
  })

  output$differentK <- renderPlotly({
    different_knn()
  })

  pred_result <- reactiveVal("Passenger Survival Result")
  
  observeEvent(input$predict_btn, {
    new_data <- data.frame(
      Pclass = as.numeric(input$pclass),
      Sex    = ifelse(input$sex == "male", 0, 1),
      Age    = input$age,
      Fare   = input$fare
    )
    
    new_scaled <- predict(preproc, new_data)
    
    new_pca <- as.data.frame(predict(pca_model, new_scaled))
    
    pred <- knn(train = train_scaled,
                test  = new_scaled,
                cl    = train$Survived,
                k     = input$kneighbors)
    
    predicted_point(list(
      PC1  = new_pca$PC1,
      PC2  = new_pca$PC2,
      pred = as.numeric(as.character(pred))
    ))

    pred_result(if (pred == 1) "✅ Survived" else "❌ Didn't Survive")
  })

    output$prediction <- renderText({
      paste0(pred_result())
    })
  
  # Dataset
  output$main_table <- renderDT({
    df <- titanic[, c("Survived", "Pclass", "Sex", "Age", "Fare")]
    df$Survived = factor(df$Survived, labels = c("No", "Yes"))
    df$Survived <- ifelse(as.character(df$Survived) == "Yes",
                          '<span style="background:rgba(52,76,101,0.15);color:#344C65;padding:3px 10px;border-radius:20px;font-size:11px;font-weight:600;">Yes</span>',
                          '<span style="background:rgba(172,108,53,0.15);color:#AC6C35;padding:3px 10px;border-radius:20px;font-size:11px;font-weight:600;">No</span>'
    )
    df$Sex = factor(df$Sex, labels = c("Male", "Female"))
    df$Fare  <- paste0("£", round(df$Fare, 2))
    df$Pclass <- paste0(df$Pclass, c("st","nd","rd")[pmin(df$Pclass, 3)])
    datatable(df, escape=FALSE, rownames=FALSE, filter="none",
              colnames=c("Survived","PClass","Sex","Age","Fare"),
              options=list(pageLength=15, dom="frtip", scrollX=TRUE),
              class="compact"
    )
  })
}

shinyApp(ui = ui, server = server)