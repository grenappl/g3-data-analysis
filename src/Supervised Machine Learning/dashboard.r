library(shiny)
library(plotly)
library(rpart)
source("C:/stuff/Code/R/New folder/g3-data-analysis/src/Supervised Machine Learning/knn.r")

ui <- fluidPage(
    titlePanel("Iris Classifier"),
    sidebarLayout(
        sidebarPanel(
            div(
                sliderInput("kneighbors", "# of Neighbours:",
                    min = 1, max = 50,
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
            htmlOutput("accuracy")
        )
    )
)
server <- function(input, output) {

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
    output$accuracy <- renderUI({
        cM <- knn_results()   # call it like a function
        tags$h3(paste("Accuracy:", round(cM$overall["Accuracy"], 3)))
    })

    observeEvent(input$predict_btn, {
        

        new_data <- data.frame(
            Pclass = as.numeric(input$pclass),
            Sex    = ifelse(input$sex == "male", 0, 1),
            Age    = input$age,
            Fare   = input$fare
        )
        
        new_scaled <- predict(preproc, new_data)
        
        pred <- knn(train  = train_scaled,
                    test   = new_scaled,
                    cl     = train$Survived,
                    k      = input$kneighbors)


        output$prediction <- renderText({
            if (pred == 1) "✅ Survived" else "❌ Did Not Survive"
        })
    })
}

shinyApp(ui, server)