library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(rpart)
library(ggplot2)

source('data.r')
source('knn_model.r')
source('group_name.r')
source('data_visuals.r')

css <- "
  @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@600;700&family=Source+Sans+3:wght@300;400;500;600&family=DM+Mono:wght@400;500&display=swap');

  *, *::before, *::after { box-sizing: border-box; }

  body, .content-wrapper, .right-side {
    background: #DBCEBF !important;
    font-family: 'Source Sans 3', sans-serif;
    color: #2A2521 !important;
  }

  /* ── Sidebar ── */
  .main-sidebar, .left-side { background: #2A2521 !important; border-right: none !important; }
  .main-header .logo { width: 230px; background: #2A2521 !important; border-bottom: 1px solid rgba(255,255,255,0.06) !important; font-family:'Playfair Display',serif; font-size:15px; color:#DBCEBF !important; }
  .main-header .navbar { background: #cdbfb0 !important; border-bottom: 1px solid #bfb09d !important; }
  .main-header .sidebar-toggle { color: #2A2521 !important; }
  .main-header .sidebar-toggle:hover { background: rgba(0,0,0,0.10) !important; }
  .main-header .logo .logo-lg { font-family: 'Playfair Display',serif; font-size:15px; color:#DBCEBF; }

  .sidebar-menu .header { color: rgba(219,206,191,0.32) !important; font-size:9.5px; font-weight:600; letter-spacing:1.6px; text-transform:uppercase; padding:20px 20px 6px !important; }
  .sidebar-menu > li > a { color: rgba(219,206,191,0.62) !important; font-size:13.5px; font-weight:500; padding:11px 20px 11px 22px !important; border-left:3px solid transparent; transition:all 0.18s ease; }
  .sidebar-menu > li > a .fa { width:20px; }
  .sidebar-menu > li > a:hover,
  .sidebar-menu > li.active > a { color: #E9BA24 !important; background: rgba(172,108,53,0.20) !important; border-left-color: #AC6C35 !important; }

  /* ── Page headings ── */
  .page-title { font-family:'Playfair Display',serif; font-size:26px; font-weight:700; color:#2A2521; margin:0 0 4px; letter-spacing:-0.3px; }
  .page-sub   { font-size:13px; color:#7a6e65; margin:0 0 18px; }
  .divider    { border:none; border-top:1px solid #c4b8aa; margin:0 0 20px; }

  /* ── Cards ── */
  .box { border-radius:10px !important; box-shadow:0 2px 8px rgba(42,37,33,0.10) !important; border:none !important; background:#e8ddd4 !important; transition: border-color 0.3s ease-out, transform 0.3s ease-out; }
  .box:hover { border-color: #2A2521 !important; cursor: pointer;  transform: scale(1.02); }
  .box-header { background:#e8ddd4 !important; border-bottom:1px solid #d0c3b5 !important; border-radius:10px 10px 0 0 !important; padding:14px 18px !important; }
  .box-title  { font-family:'Playfair Display',serif !important; font-size:15px !important; font-weight:600 !important; color:#2A2521 !important; }
  .box-body   { padding:16px 18px !important; }

  /* ── KPI tiles ── */
  .kpi-row { display:flex; gap:16px; flex-wrap:wrap; margin-bottom:22px; }
  .kpi-tile { flex:1; min-width:150px; border-radius:10px; padding:18px 20px; color:#fff; box-shadow:0 2px 10px rgba(42,37,33,0.18); }
  .kpi-val  { font-family:'DM Mono',monospace; font-size:28px; font-weight:500; }
  .kpi-lbl  { font-size:11px; font-weight:600; letter-spacing:0.6px; text-transform:uppercase; opacity:0.82; margin-top:5px; }
  .kpi-icon { font-size:40px; opacity:0.18; float:right; margin-top:-10px; }
  .kpi-dark  { background:#443d37; }
  .kpi-navy  { background:#344C65; }
  .kpi-amber { background:#AC6C35; }
  .kpi-gold  { background:#b89018; }

  /* ── Metrics pills ── */
  .metric-row  { display:flex; gap:12px; flex-wrap:wrap; margin-bottom:14px; }
  .metric-pill { background:#DBCEBF; border-radius:8px; padding:12px 16px; flex:1; min-width:100px; text-align:center; box-shadow:0 1px 4px rgba(42,37,33,0.10); }
  .metric-val  { font-family:'DM Mono',monospace; font-size:20px; font-weight:500; color:#344C65; }
  .metric-lbl  { font-size:10px; font-weight:600; text-transform:uppercase; letter-spacing:0.7px; color:#7a6e65; margin-top:2px; }

  /* ── Prediction ── */
  .pred-form { background:#e8ddd4; border-radius:10px; padding:22px; box-shadow:0 2px 8px rgba(42,37,33,0.10); }
  .pred-form label { font-size:13px; font-weight:600; color:#2A2521; }
  .pred-form .form-control, .pred-form select { background:#DBCEBF !important; border:1px solid #c5b9ac !important; border-radius:7px !important; color:#2A2521 !important; font-size:14px; }
  .pred-btn { background:#344C65 !important; color:#fff !important; border:none; border-radius:8px; padding:10px 28px; font-size:14px; font-weight:600; cursor:pointer; width:100%; margin-top:8px; letter-spacing:0.2px; transition:opacity .18s; }
  .pred-btn:hover { opacity:0.88; }
  .pred-result { padding:22px 24px; border-radius:10px; text-align:center; }
  .pred-result .result-label { font-family:'Playfair Display',serif; font-size:24px; font-weight:700; }
  .pred-result .result-sub   { font-size:13px; margin-top:6px; opacity:0.80; line-height:1.55; }
  .survived-yes { background:rgba(52,76,101,0.14); border:2px solid #344C65; color:#344C65; }
  .survived-no  { background:rgba(172,108,53,0.14); border:2px solid #AC6C35; color:#2A2521; }
  .pred-placeholder { background:#e8ddd4; border-radius:10px; padding:32px 24px; text-align:center; color:#9a8e86; font-size:14px; box-shadow:0 2px 8px rgba(42,37,33,0.08); }

  /* ── Confusion Matrix ── */
  .cm-wrap { overflow-x:auto; }
  .cm-table { width:100%; border-collapse:collapse; font-family:'DM Mono',monospace; font-size:13px; }
  .cm-table th, .cm-table td { padding:10px 14px; text-align:center; border:1px solid #c5b9ac; }
  .cm-table .hdr { background:#344C65; color:#DBCEBF; font-weight:600; font-size:11px; letter-spacing:0.5px; }
  .cm-table .row-hdr { background:#DBCEBF; color:#2A2521; font-weight:600; font-size:11px; letter-spacing:0.5px; }
  .cm-table .tp { background:rgba(52,76,101,0.20); color:#344C65; font-weight:700; font-size:16px; }
  .cm-table .tn { background:rgba(52,76,101,0.20); color:#344C65; font-weight:700; font-size:16px; }
  .cm-table .fp { background:rgba(172,108,53,0.20); color:#AC6C35; font-weight:700; font-size:16px; }
  .cm-table .fn { background:rgba(172,108,53,0.20); color:#AC6C35; font-weight:700; font-size:16px; }

  /* ── DataTable ── */
  .dataTables_wrapper, table.dataTable { color:#2A2521 !important; font-size:13px; }
  table.dataTable thead th { background:#344C65 !important; color:#DBCEBF !important; border:none !important; font-size:11px; font-weight:600; text-transform:uppercase; letter-spacing:0.8px; padding:10px 14px !important; }
  table.dataTable tbody tr { background:#e8ddd4 !important; }
  table.dataTable tbody tr:hover { background:#ddd0c4 !important; }
  table.dataTable tbody td { border-color:#d0c3b5 !important; padding:9px 14px !important; }
  .dataTables_info, .dataTables_length label, .dataTables_filter label { color:#7a6e65 !important; font-size:12px; }
  .dataTables_paginate .paginate_button { color:#2A2521 !important; }
  .dataTables_paginate .paginate_button.current { background:#344C65 !important; color:#fff !important; border-radius:6px !important; border:none !important; }
  select, input[type=search] { background:#DBCEBF !important; color:#2A2521 !important; border:1px solid #c5b9ac !important; border-radius:6px !important; }

  /* ── Team ── */
  .team-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(210px,1fr)); gap:20px; }
  .team-card { background:#e8ddd4; border-radius:12px; padding:28px 20px 22px; text-align:center; box-shadow:0 2px 8px rgba(42,37,33,0.10); }
  .team-avatar { width:72px; height:72px; border-radius:50%; margin:0 auto 14px; display:flex; align-items:center; justify-content:center; font-family:'Playfair Display',serif; font-size:26px; font-weight:700; color:#fff; }
  .team-name { font-family:'Playfair Display',serif; font-size:16px; font-weight:600; color:#2A2521; }
  .team-role { font-size:11px; color:#AC6C35; font-weight:600; margin-top:4px; text-transform:uppercase; letter-spacing:0.7px; }
  .team-bio  { font-size:13px; color:#6b5f58; margin-top:10px; line-height:1.55; }

  ::-webkit-scrollbar { width:6px; height:6px; }
  ::-webkit-scrollbar-track { background:#DBCEBF; }
  ::-webkit-scrollbar-thumb { background:#bfb3a5; border-radius:3px; }
"

kpi <- function(value, label, icon_name, css_class) {
  div(class = paste("kpi-tile", css_class),
    div(style = "padding-top: 10px;",
      div(class = "kpi-icon", icon(icon_name)),
      div(class = "kpi-val",  value),
      div(class = "kpi-lbl",  label)
    )
  )
}

ptly <- function(p) {
  p |> layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font   = list(family = "Source Sans 3", color = "#6b5f58", size = 12),
    xaxis  = list(gridcolor = "#cbbfb2", zerolinecolor = "#cbbfb2", color = "#6b5f58"),
    yaxis  = list(gridcolor = "#cbbfb2", zerolinecolor = "#cbbfb2", color = "#6b5f58"),
    legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#6b5f58")),
    margin = list(l = 40, r = 28, t = 28, b = 40)
  )
}

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
          box(title = "Fare Distribution by Passenger Class", width = 7,
            plotlyOutput("an_fare", height = "280px")
          ),
          box(title = "Survival Rate by Class and Sex", width = 5,
            plotlyOutput("an_heatmap", height = "280px")
          )
        ),
        fluidRow(
          box(title = "Age vs Fare - Coloured by Survival", width = 8,
            plotlyOutput("an_scatter", height = "270px")
          )
        ),
        fluidRow(
          box(title = "Fare Distribution by Survival", width = 12,
            plotlyOutput("an_box", height = "300px")
          )
        )
      ),

      # PREDICTIONS
      tabItem(tabName = "predictions",
        tags$h2("Predictions", class = "page-title"),
        tags$hr(class = "divider"),

        # YOU CAN USE THIS HERE
        # box(title = "Model Performance", width = 5,
        #   div(class = "metric-row",
        #     div(class = "metric-pill", div(class="metric-val", paste0(accuracy,"%")),   div(class="metric-lbl","Accuracy")),
        #     div(class = "metric-pill", div(class="metric-val", paste0(sensitivity,"%")),div(class="metric-lbl","Sensitivity")),
        #     div(class = "metric-pill", div(class="metric-val", paste0(specificity,"%")),div(class="metric-lbl","Specificity"))
        #   ),
        #   tags$p(style = "font-size:13px; color:#7a6e65; line-height:1.65; margin-top:8px;",
        #     "A KNN model (k=5) was trained on 70% of the cleaned Titanic dataset ",
        #     "and evaluated on the remaining 30%. Features: Passenger Class, Sex, Age, ",
        #     "and Fare — all z-score normalised before training."
        #   )
        # )

        fluidPage(
          titlePanel("KNN"),
          sidebarLayout(
              sidebarPanel(
                  div(
                      sliderInput("kneighbors", "# of Neighbours:",
                          min = 1, max = 150,
                          value = 3),
                  ),
                  div(
                      selectInput("pclass", "Passenger Class", choices = c(1, 2, 3)),
                      selectInput("sex", "Sex", choices = c("male", "female")),
                      numericInput("age", "Age", value = 30, min = 1, max = 100),
                      numericInput("fare", "Fare", value = 32, min = 0, max = 600),
                      actionButton("predict_btn", "Predict", class = "btn-primary")
                  ),
              ),
              mainPanel(
                  h3("Prediction:"),
                  verbatimTextOutput("prediction"),
                  htmlOutput("metrics"),
                  h4("PCA decision boundary – PC1 vs PC2"),
                  plotlyOutput("pca_boundary"),
                  plotlyOutput("accuracy_k_plot")
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


  ## JOE STUFF
  predicted_point <- reactiveVal(NULL)

    knn_results <- reactive({
        train_scaled <- predict(preproc, train[, numeric_cols])
        test_scaled_full <- predict(preproc, test[, numeric_cols])
        
        pred_all <- knn(train = train_scaled,
                        test  = test_scaled_full,
                        cl    = train$Survived,
                        k     = input$kneighbors)   
        
        confusionMatrix(pred_all, test$Survived)
    })

    # Use it in outputs — updates whenever slider moves
    output$metrics <- renderUI({
        metrics(knn_results())

    })

    output$pca_boundary <- renderPlotly({PCA_boundary(input$kneighbors, predicted_point())})

    output$accuracy_k_plot <- renderPlotly({
      K_accuracy_plot(input$kneighbors)
    })
    

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

        output$prediction <- renderText({
            if (pred == 1) "Survived" else "Did Not Survive"
        })
    })

  # Dataset
  output$main_table <- renderDT({
    df <- titanic
    df$Survived = factor(df$Survived, labels = c("No", "Yes"))
    df$Survived <- ifelse(as.character(df$Survived) == "Yes",
      '<span style="background:rgba(52,76,101,0.15);color:#344C65;padding:3px 10px;border-radius:20px;font-size:11px;font-weight:600;">Yes</span>',
      '<span style="background:rgba(172,108,53,0.15);color:#AC6C35;padding:3px 10px;border-radius:20px;font-size:11px;font-weight:600;">No</span>'
    )
    df$Sex = factor(df$Sex, labels = c("Male", "Female"))
    # df$Sex   <- tools::toTitleCase(df$Sex)
    df$Fare  <- paste0("£", round(df$Fare, 2))
    df$Pclass <- paste0(df$Pclass, c("st","nd","rd")[pmin(df$Pclass, 3)])
    datatable(df, escape=FALSE, rownames=FALSE,
      colnames=c("Survived","PClass","Sex","Age","Fare"),
      options=list(pageLength=15, dom="frtip", scrollX=TRUE),
      class="compact"
    )
  })
}

shinyApp(ui = ui, server = server)
