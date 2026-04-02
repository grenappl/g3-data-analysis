library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(rpart)
library(ggplot2)
source('data.r')

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
    df <- as.data.frame(table(Pclass = titanic$Pclass, Survived = titanic$Survived))
    df$Survived <- factor(df$Survived, labels = c("No", "Yes"))
    df$Pclass <- factor(df$Pclass, labels = c("1st","2nd","3rd"))
    plot_ly(df, x=~Pclass, y=~Freq, color=~Survived,
            colors = c(COL_AMBER, COL_NAVY),
            type="bar", barmode="group",
            marker=list(line=list(width=0))) |> ptly()
  })

  output$ov_sex <- renderPlotly({
    df <- as.data.frame(table(Sex = titanic$Sex, Survived = titanic$Survived))
    df$Survived <- factor(df$Survived, labels = c("No", "Yes"))
    df$Sex <- factor(df$Sex, labels=c("Male","Female"))
    plot_ly(df, x=~Sex, y=~Freq, color=~Survived,
            colors = c(COL_AMBER, COL_NAVY),
            type="bar", barmode="stack",
            marker=list(line=list(width=0))) |> ptly()
  })

  Age <- titanic$Age[titanic$Survived == 0]

  output$ov_age <- renderPlotly({
    plot_ly(alpha=0.6) |>
      add_histogram(x = ~Age,  name="No",
                    marker=list(color=COL_AMBER)) |>
      add_histogram(x = ~titanic$Age[titanic$Survived == 1], name="Yes",
                    marker=list(color=COL_NAVY)) |>
      layout(barmode="overlay") |> ptly()
  })

  output$an_pie <- renderPlotly({
    df <- as.data.frame(table(Class=titanic$Pclass))
    df$Class <- paste(c("1st","2nd","3rd"))
    plot_ly(df, labels=~Class, values=~Freq, type="pie",
            marker=list(colors=c(COL_NAVY, COL_AMBER, COL_GOLD),
                        line=list(color=COL_CREAM, width=2)),
            textfont=list(color="#fff"), sort = FALSE) |>
      ptly() |> layout(showlegend=TRUE)
  })

  # Analytics
  output$an_fare <- renderPlotly({
    plot_ly(titanic, y=~Fare, x=~factor(Pclass, labels=c("1st","2nd","3rd")),
            type="box", split=~factor(Pclass),
            colors=c(COL_NAVY, COL_AMBER, COL_GOLD),
            box=list(visible=TRUE), meanline=list(visible=TRUE)) |>
      ptly() |> layout(showlegend=FALSE,
        xaxis=list(title="PClass"), yaxis=list(title="Fare (£)"))
  })

  output$an_heatmap <- renderPlotly({
    df <- aggregate(as.numeric(Survived=="Yes") ~ Pclass + Sex, data=titanic, FUN=mean)
    colnames(df) <- c("Class","Sex","Rate")
    df$Label <- paste0(round(df$Rate*100), "%")
    df$Sex   <- factor(df$Sex, levels=c("male","female"), labels=c("Male","Female"))
    df$Class <- factor(df$Class, labels=c("1st","2nd","3rd"))
    plot_ly(df, x=~Sex, y=~Class, z=~Rate, type="heatmap",
            colorscale=list(c(0,COL_CREAM), c(0.5,COL_AMBER), c(1,COL_NAVY)),
            text=~Label, texttemplate="%{text}", showscale=FALSE) |>
      ptly() |> layout(xaxis=list(title=""), yaxis=list(title=""))
  })

  output$an_scatter <- renderPlotly({
    plot_ly(titanic, x=~Age, y=~Fare, color=~Survived,
            colors=c(COL_AMBER, COL_NAVY),
            type="scatter", mode="markers",
            marker=list(size=6, opacity=0.65,
                        line=list(width=0.5, color="rgba(255,255,255,0.4)")),
            text=~paste("Class:", Pclass, "<br>Sex:", Sex)) |>
      ptly() |> layout(xaxis=list(title="Age"), yaxis=list(title="Fare (£)"))
  })

  output$an_box <- renderPlotly({
    plot_ly(titanic, x=~Survived, y=~Fare, color=~Survived,
            colors=c(COL_AMBER, COL_NAVY), type="box") |>
      ptly() |> layout(showlegend=FALSE,
        xaxis=list(title="Survived"), yaxis=list(title="Fare (£)"))
  })

  # Predictions
  output$pred_result_ui <- renderUI({
    div(class="pred-placeholder",
      icon("compass", style="font-size:28px; color:#bfb3a5; margin-bottom:10px; display:block;"),
      "Fill in the passenger details on the left and click",
      tags$strong(" Predict Survival"), " to see the model's verdict."
    )
  })

  observeEvent(input$predict_btn, {
    new_df <- data.frame(
      Pclass  = as.numeric(input$p_class),
      Sex_num = ifelse(input$p_sex == "male", 0, 1),
      Age     = as.numeric(input$p_age),
      Fare    = as.numeric(input$p_fare)
    )
    new_scaled <- predict(preproc, new_df)
    colnames(new_scaled) <- numeric_cols

    pred     <- knn(train=train_scaled, test=new_scaled, cl=train$Survived, k=k)
    survived <- as.character(pred) == "Yes"

    css_cls  <- if (survived) "pred-result survived-yes" else "pred-result survived-no"
    label    <- if (survived) "✓  Likely Survived" else "✗  Unlikely to Survive"
    class_lbl <- c("1"="1st","2"="2nd","3"="3rd")[input$p_class]
    detail   <- sprintf("Class: %s  ·  %s  ·  Age: %d  ·  Fare: £%d  ·  k=%d",
                        class_lbl, tools::toTitleCase(input$p_sex),
                        input$p_age, input$p_fare, k)

    output$pred_result_ui <- renderUI({
      div(class=css_cls,
        div(class="result-label", label),
        div(class="result-sub",   detail)
      )
    })
  })

  output$cm_table_ui <- renderUI({
    tbl <- cm$table
    tp  <- tbl["Yes","Yes"]; tn <- tbl["No","No"]
    fp  <- tbl["Yes","No"];  fn <- tbl["No","Yes"]
    HTML(sprintf("
      <table class='cm-table'>
        <thead>
          <tr>
            <th colspan='2' style='background:#DBCEBF;border:1px solid #c5b9ac;'></th>
            <th class='hdr' colspan='2'>Actual</th>
          </tr>
          <tr>
            <th style='background:#DBCEBF;border:1px solid #c5b9ac;'></th>
            <th style='background:#DBCEBF;border:1px solid #c5b9ac;'></th>
            <th class='hdr'>Not Survived</th>
            <th class='hdr'>Survived</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td class='row-hdr' rowspan='2'
              style='writing-mode:vertical-lr;transform:rotate(180deg);padding:14px 8px;letter-spacing:1px;font-size:10px;text-transform:uppercase;'>
              Predicted
            </td>
            <td class='row-hdr'>Not Survived</td>
            <td class='tn'>%d</td>
            <td class='fn'>%d</td>
          </tr>
          <tr>
            <td class='row-hdr'>Survived</td>
            <td class='fp'>%d</td>
            <td class='tp'>%d</td>
          </tr>
        </tbody>
      </table>", tn, fn, fp, tp))
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
            cm <- knn_results()   # call it like a function
            accuracy    <- round(cm$overall["Accuracy"], 3)
            sensitivity <- round(cm$byClass["Sensitivity"], 3)
            specificity <- round(cm$byClass["Specificity"], 3)
            f1          <- round(cm$byClass["F1"], 3)

            TP <- cm$table[1, 1]
            FP <- cm$table[1, 2]
            FN <- cm$table[2, 1]
            TN <- cm$table[2, 2]

            # Build data frame
            metrics_df <- data.frame(
                Metric = c("Accuracy", "Sensitivity", "Specificity", "F1 Score", "TP", "TN", "FP", "FN"),
                Value  = c(accuracy, sensitivity, specificity, f1, TP, TN, FP, FN)
            )
            tags$table(
                style = "width: 100%; border-collapse: collapse; text-align: center;",
                tags$thead(
                tags$tr(
                    tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #4A90D9; color: white;", "Metric"),
                    tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #4A90D9; color: white;", "Value")
                )
                ),
                tags$tbody(
                lapply(1:nrow(metrics_df), function(i) {
                    bg <- ifelse(i %% 2 == 0, "#f9f9f9", "white")
                    tags$tr(
                    tags$td(style = paste0("border: 1px solid #ddd; padding: 8px; background-color:", bg, ";"), metrics_df$Metric[i]),
                    tags$td(style = paste0("border: 1px solid #ddd; padding: 8px; background-color:", bg, ";"), metrics_df$Value[i])
                    )
                })
                )
            )

        })

        output$pca_boundary <- renderPlotly({
            pc1_seq <- seq(min(train_pca$PC1) - 0.5, max(train_pca$PC1) + 0.5, length.out = 150)
            pc2_seq <- seq(min(train_pca$PC2) - 0.5, max(train_pca$PC2) + 0.5, length.out = 150)
    
            grid_pca  <- expand.grid(PC1 = pc1_seq, PC2 = pc2_seq)
    
            grid_pred <- knn(train = train_pca[, c("PC1", "PC2")],
                            test  = grid_pca,
                            cl    = train$Survived,
                            k     = input$kneighbors)
    
            z_matrix <- matrix(as.numeric(as.character(grid_pred)),
                            nrow = length(pc2_seq),
                            ncol = length(pc1_seq))
    
            test_pca_plot <- data.frame(test_pca, Survived = test$Survived)
    
            p <- plot_ly() %>%
                add_contour(x = pc1_seq, y = pc2_seq, z = z_matrix,
                            colorscale = list(c(0, "#F4CCCC"), c(1, "#C6D9F0")),
                            contours   = list(start = 0, end = 1, size = 0.5,
                                            coloring = "fill", showlines = TRUE),
                            line       = list(color = "black", width = 1.5),
                            showscale  = FALSE, opacity = 0.6) %>%
                add_markers(data   = test_pca_plot[test_pca_plot$Survived == 0, ],
                            x = ~PC1, y = ~PC2,
                            marker = list(color = "#CC0000", size = 6,
                                        line = list(color = "white", width = 0.5)),
                            name   = "Did Not Survive") %>%
                add_markers(data   = test_pca_plot[test_pca_plot$Survived == 1, ],
                            x = ~PC1, y = ~PC2,
                            marker = list(color = "#1A6FBF", size = 6,
                                        line = list(color = "white", width = 0.5)),
                            name   = "Survived")
    
            # ── Overlay predicted passenger as a star if button was clicked ────
            pt <- predicted_point()
            if (!is.null(pt)) {
                dot_color <- if (pt$pred == 1) "#1A6FBF" else "#CC0000"
                dot_label <- if (pt$pred == 1) "Your Passenger (Survived)" else "Your Passenger (Did Not Survive)"
    
                p <- p %>%
                    add_markers(x = pt$PC1, y = pt$PC2,
                                marker = list(
                                    color  = dot_color,
                                    size   = 16,
                                    symbol = "star",
                                    line   = list(color = "black", width = 1.5)
                                ),
                                name = dot_label)
            }
    
            p %>% layout(
                title  = paste0("PCA Decision Boundary (k = ", input$kneighbors, ")"),
                xaxis  = list(title = "PC1"),
                yaxis  = list(title = "PC2")
            )
        })

        output$accuracy_k_plot <- renderPlotly({
            train_scaled <- predict(preproc, train[, numeric_cols])
            test_scaled  <- predict(preproc, test[, numeric_cols])
            
            k_values  <- seq(1, 200, by = 5)
            accuracies <- sapply(k_values, function(k) {
                pred <- knn(train = train_scaled,
                            test  = test_scaled,
                            cl    = train$Survived,
                            k     = k)
                mean(pred == test$Survived)
            })
            
            acc_df <- data.frame(K = k_values, Accuracy = accuracies)
            
            plot_ly(acc_df, x = ~K, y = ~Accuracy, type = "scatter", mode = "lines+markers",
                    line   = list(color = "#1A6FBF", width = 2),
                    marker = list(color = "#1A6FBF", size = 5)) %>%
                # Highlight current k
                add_segments(x = input$kneighbors, xend = input$kneighbors,
                            y = min(accuracies), yend = max(accuracies),
                            line = list(color = "#CC0000", dash = "dash", width = 2),
                            name = paste("Current k =", input$kneighbors)) %>%
                layout(title = "Accuracy vs K",
                    xaxis = list(title = "K (Neighbors)"),
                    yaxis = list(title = "Accuracy", tickformat = ".0%"),
                    showlegend = TRUE)
        })
        

        observeEvent(input$predict_btn, {
            new_data <- data.frame(
                Pclass = as.numeric(input$pclass),
                Sex    = ifelse(input$sex == "male", 0, 1),
                Age    = input$age,
                Fare   = input$fare
            )
    
            # 1. Scale using the same preproc as training
            new_scaled <- predict(preproc, new_data)
    
            # 2. Project into PCA space using the same pca_model
            new_pca <- as.data.frame(predict(pca_model, new_scaled))
    
            # 3. Predict using full 4-feature KNN (not PCA-reduced)
            pred <- knn(train = train_scaled,
                        test  = new_scaled,
                        cl    = train$Survived,
                        k     = input$kneighbors)
    
            # 4. Save PCA coords + prediction into reactiveVal
            #    This triggers pca_boundary to re-render with the star
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
