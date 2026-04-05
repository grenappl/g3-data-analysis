    library(shiny)
    library(plotly)
    library(rpart)
    # setwd here
    source("C:/stuff/Code/R/New folder/g3-data-analysis/src/Supervised Machine Learning/knn.r")
    ui <- fluidPage(
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
    server <- function(input, output) {

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
    }

    shinyApp(ui, server)
