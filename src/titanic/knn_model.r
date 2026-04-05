library(titanic)
library(class)
library(caret)
library(plotly)
data(titanic_train)
titanic <- titanic_train[,
    c("Survived", "Pclass", "Sex", "Age", "Fare")]
titanic <- na.omit(titanic)

titanic$Survived <- factor(titanic$Survived)
titanic$Sex <- ifelse(titanic$Sex == "male", 0, 1)
# str(titanic)
set.seed(33)
train_index = createDataPartition(titanic$Survived, p =0.7, list=FALSE)
train <- titanic[train_index, ]
test <- titanic[-train_index, ]

# Scaling
numeric_cols <- c("Pclass", "Sex", "Age", "Fare")
preproc <- preProcess(train[, numeric_cols], method =  c("center", "scale"))

train_scaled <- predict(preproc, train[, numeric_cols])
test_scaled <- predict(preproc, test[, numeric_cols])

# PCA Model
pca_model <- prcomp(train_scaled, center = FALSE, scale. = FALSE)
train_pca <- as.data.frame(predict(pca_model, train_scaled))
test_pca  <- as.data.frame(predict(pca_model, test_scaled))

# VISUALS

PCA_boundary <- function (kneighbors, predicted_point){
    pc1_seq <- seq(min(train_pca$PC1) - 0.5, max(train_pca$PC1) + 0.5, length.out = 150)
    pc2_seq <- seq(min(train_pca$PC2) - 0.5, max(train_pca$PC2) + 0.5, length.out = 150)

    grid_pca  <- expand.grid(PC1 = pc1_seq, PC2 = pc2_seq)

    grid_pred <- knn(train = train_pca[, c("PC1", "PC2")],
                    test  = grid_pca,
                    cl    = train$Survived,
                    k     = kneighbors)

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
                    name   = "Survived") %>% ptly()

    # ── Overlay predicted passenger as a star if button was clicked ────
    pt <- predicted_point
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
        title  = paste0("PCA Decision Boundary (k = ", kneighbors, ")"),
        xaxis  = list(title = "PC1"),
        yaxis  = list(title = "PC2")
    )
}

different_knn <- function (){
    pc1_seq  <- seq(min(train_pca$PC1) - 0.5, max(train_pca$PC1) + 0.5, length.out = 150)
  pc2_seq  <- seq(min(train_pca$PC2) - 0.5, max(train_pca$PC2) + 0.5, length.out = 150)
  grid_pca <- expand.grid(PC1 = pc1_seq, PC2 = pc2_seq)
  
  make_subplot <- function(k, show_legend) {
    
    # Predict on grid
    grid_pred <- knn(
      train = train_pca[, c("PC1", "PC2")],
      test  = grid_pca,
      cl    = train$Survived,
      k     = k
    )
    
    z_matrix <- matrix(
      as.numeric(as.character(grid_pred)),  # 0 or 1
      nrow = length(pc1_seq),
      ncol = length(pc2_seq)
    )
    
    test_pca_plot <- data.frame(test_pca, Survived = test$Survived)
    
    plot_ly() %>%
      add_contour(
        x          = pc1_seq,
        y          = pc2_seq,
        z          = z_matrix,
        colorscale = list(c(0, "#F4CCCC"), c(1, "#C6D9F0")),
        contours   = list(
          start    = 0, end = 1, size = 0.5,
          coloring = "fill", showlines = TRUE
        ),
        line      = list(color = "black", width = 1.5),
        showscale = FALSE,
        opacity   = 0.6
      ) %>%
      add_markers(
        data   = test_pca_plot[test_pca_plot$Survived == 0, ],
        x      = ~PC1, y = ~PC2,
        marker = list(color = "#CC0000", size = 6,
                      line = list(color = "white", width = 0.5)),
        name   = "Did Not Survive",
        legendgroup = "no",
        showlegend = show_legend
      ) %>%
      add_markers(
        data   = test_pca_plot[test_pca_plot$Survived == 1, ],
        x      = ~PC1, y = ~PC2,
        marker = list(color = "#1A6FBF", size = 6,
                      line = list(color = "white", width = 0.5)),
        name   = "Survived",
        legendgroup = "yes",
        showlegend = show_legend
      ) %>%
      layout(
        title  = paste0("2-Class Classification (k = ", k, ")"),
        xaxis  = list(title = "PC1"),
        yaxis  = list(title = "PC2")
      ) %>% ptly()
  }
  
  p1 <- make_subplot(k = 1,  show_legend = TRUE)
  p2 <- make_subplot(k = 9,  show_legend = FALSE)
  p3 <- make_subplot(k = 18, show_legend = FALSE)
  
  subplot(
    list(p1, p2, p3),
    nrows       = 1,
    shareX      = FALSE,
    shareY      = FALSE,
    titleX      = TRUE,
    titleY      = TRUE,
    margin      = 0.05
  ) %>%
    layout(
      title = list(
        text = paste(
          "<b>Larger k = less complex = underfitting</b>",
          "<b>Smaller k = more complex = overfitting</b>",
          sep = "<br>"
        ),
        font = list(size = 13),
        y = 0.95
      ),
      margin = list(t = 80),
      annotations = list(
        list(x = 0.12, y = 1.05, text = "<b>k = 1</b>",  showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 13)),
        list(x = 0.50, y = 1.05, text = "<b>k = 9</b>",  showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 13)),
        list(x = 0.88, y = 1.05, text = "<b>k = 18</b>", showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 13))
      )
    )
}

model_qual <- function(cm){
    accuracy    <- round(cm$overall["Accuracy"], 3)
    sensitivity <- round(cm$byClass["Sensitivity"], 3)
    specificity <- round(cm$byClass["Specificity"], 3)
    f1          <- round(cm$byClass["F1"], 3)

      # Build data frame
    metrics_df <- data.frame(
        Metric = c("Accuracy", "Sensitivity", "Specificity", "F1 Score"),
        Value  = c(accuracy, sensitivity, specificity, f1)
    )

    div(
      div(class = "kpi-row",
        kpi(metrics_df$Value[1], metrics_df$Metric[1], "crosshairs", "kpi-dark"),
        kpi(metrics_df$Value[2], metrics_df$Metric[2], "disease", "kpi-navy")
      ),
      div(class = "kpi-row",
        kpi(metrics_df$Value[3], metrics_df$Metric[3], "hand-point-left", "kpi-orange"),
        kpi(metrics_df$Value[4], metrics_df$Metric[4], "arrow-trend-up", "kpi-gold")
      )
    )
}

conf_mat <- function(cm){
    TP <- cm$table[1, 1]
    FP <- cm$table[1, 2]
    FN <- cm$table[2, 1]
    TN <- cm$table[2, 2]

    metrics_df <- data.frame(
        Metric = c("True Positive", "False Positive", "False Negative", "True Negative"),
        Value  = c(TP, FP, FN, TN)
    )

    div(
      div(class = "kpi-row",
        kpi(metrics_df$Value[1], metrics_df$Metric[1], "circle-plus", "kpi-dark"),
        kpi(metrics_df$Value[2], metrics_df$Metric[2], "square-plus", "kpi-navy")
      ),
      div(class = "kpi-row",
        kpi(metrics_df$Value[3], metrics_df$Metric[3], "square-minus", "kpi-orange"),
        kpi(metrics_df$Value[4], metrics_df$Metric[4], "circle-minus", "kpi-gold")
      )
    )
}

K_accuracy_plot <- function (kneighbors){
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
        add_segments(x = kneighbors, xend = kneighbors,
                    y = min(accuracies), yend = max(accuracies),
                    line = list(color = "#CC0000", dash = "dash", width = 2),
                    name = paste("Current k =", kneighbors)) %>%
        layout(title = "Accuracy vs K",
            xaxis = list(title = "K (Neighbors)"),
            yaxis = list(title = "Accuracy", tickformat = ".0%"),
            showlegend = TRUE) %>% ptly()
}
