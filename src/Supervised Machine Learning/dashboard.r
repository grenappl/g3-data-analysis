library(shiny)
library(plotly)
library(rpart)
source("C:/stuff/Code/R/New folder/g3-data-analysis/src/Supervised Machine Learning/knn.r")
model <- rpart(Species ~ ., iris, method = "class")

ui <- fluidPage(
    titlePanel("Iris Classifier"),
    sidebarLayout(
        sidebarPanel(
            numericInput("sl","Sepal Length",5),
            numericInput("sw","Sepal Width",3.5),
            numericInput("pl","Petal Length",1.4),
            numericInput("pw","Petal Width",0.2),
            actionButton("go","Predict")
        ),
        mainPanel(
            textOutput("pred"),
            plotlyOutput("plot")
        )
    )
)
server <- function(input, output) {
    observeEvent(input$go, {
        x <- data.frame(
            Sepal.Length=input$sl,
            Sepal.Width=input$sw,
            Petal.Length=input$pl,
            Petal.Width=input$pw
        )
        output$pred <- renderText(
            paste("Predicted Species:", predict(model, x, type="class"))
        )
        output$plot <- renderPlotly(
            plot_ly(iris, x=~Sepal.Length, y=~Petal.Length,
                    color=~Species, type="scatter", mode="markers") %>%
            add_markers(x=input$sl, y=input$pl, inherit=FALSE)
        )
    })
}

shinyApp(ui, server)