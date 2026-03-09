library(shiny)
library(shinydashboard)
library(tidyverse)
library(cluster)
library(factoextra)
library(DT)

# UI -------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Iris Clustering Dashboard"),
  
  dashboardSidebar(
    sliderInput("clusters",
                "Select Number of Clusters (k):",
                min = 2,
                max = 10,
                value = 3),
    
    actionButton("run", "Run Clustering"),
    br(), br(),
    
    downloadButton("downloadData", "Download Cluster Summary")
  ),
  
  dashboardBody(
    
    fluidRow(
      valueBoxOutput("avgSil"),
      valueBoxOutput("totalObs")
    ),
    
    # ???? Added Interpretation Scale Box
    fluidRow(
      box(
        title = "Silhouette Score Interpretation Scale",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        HTML("
        <ul>
          <li><b>0.71 - 1.00</b> : Excellent Clustering</li>
          <li><b>0.51 - 0.70</b> : Good Clustering</li>
          <li><b>0.26 - 0.50</b> : Moderate Clustering</li>
          <li><b>Below 0.25</b> : Poor Clustering</li>
        </ul>
        ")
      )
    ),
    
    fluidRow(
      box(title = "Cluster Summary Statistics",
          width = 12,
          DTOutput("summaryTable"))
    ),
    
    fluidRow(
      box(title = "Elbow Method",
          width = 6,
          plotOutput("elbowPlot")),
      
      box(title = "Silhouette Plot",
          width = 6,
          plotOutput("silPlot"))
    ),
    
    fluidRow(
      box(title = "PCA-based Clustering",
          width = 12,
          plotOutput("pcaPlot"))
    )
  )
)

# SERVER -------------------------------------------------------------

server <- function(input, output) {
  
  data <- reactive({
    scale(iris[, 1:4])
  })
  
  kmeans_model <- eventReactive(input$run, {
    set.seed(123)
    kmeans(data(), centers = input$clusters, nstart = 25)
  })
  
  clustered_data <- reactive({
    req(kmeans_model())
    iris_clustered <- iris
    iris_clustered$Cluster <- as.factor(kmeans_model()$cluster)
    iris_clustered
  })
  
  # Summary Table
  output$summaryTable <- renderDT({
    req(clustered_data())
    clustered_data() %>%
      group_by(Cluster) %>%
      summarise(
        Count = n(),
        Sepal_Length_Mean = mean(Sepal.Length),
        Sepal_Width_Mean  = mean(Sepal.Width),
        Petal_Length_Mean = mean(Petal.Length),
        Petal_Width_Mean  = mean(Petal.Width)
      )
  })
  
  # Elbow Plot
  output$elbowPlot <- renderPlot({
    fviz_nbclust(data(), kmeans, method = "wss") +
      ggtitle("Elbow Method")
  })
  
  # Silhouette Plot
  output$silPlot <- renderPlot({
    req(kmeans_model())
    sil <- silhouette(kmeans_model()$cluster, dist(data()))
    fviz_silhouette(sil)
  })
  
  # PCA Plot
  output$pcaPlot <- renderPlot({
    req(kmeans_model())
    fviz_cluster(kmeans_model(),
                 data = data(),
                 geom = "point",
                 ellipse.type = "convex",
                 ggtheme = theme_minimal())
  })
  
  # ???? Updated Value Box with Interpretation
  output$avgSil <- renderValueBox({
    req(kmeans_model())
    
    sil <- silhouette(kmeans_model()$cluster, dist(data()))
    avg <- round(mean(sil[, 3]), 3)
    
    # Interpretation Logic
    if (avg >= 0.71) {
      interpretation <- "Excellent Clustering"
      box_color <- "green"
    } else if (avg >= 0.51) {
      interpretation <- "Good Clustering"
      box_color <- "blue"
    } else if (avg >= 0.26) {
      interpretation <- "Moderate Clustering"
      box_color <- "yellow"
    } else {
      interpretation <- "Poor Clustering"
      box_color <- "red"
    }
    
    valueBox(
      value = avg,
      subtitle = interpretation,
      icon = icon("chart-line"),
      color = box_color
    )
  })
  
  output$totalObs <- renderValueBox({
    valueBox(nrow(iris), "Total Observations", icon = icon("database"), color = "aqua")
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cluster_summary.csv", sep = "")
    },
    content = function(file) {
      write.csv(
        clustered_data() %>%
          group_by(Cluster) %>%
          summarise_all(mean),
        file,
        row.names = FALSE
      )
    }
  )
}

# Run App
shinyApp(ui, server)