library(titanic)
library(class)
library(caret)
library(plotly)
library(ggplot2)
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
preproc <- preProcess(train[, numeric_cols], method = c("center", "scale"))

train_scaled <- predict(preproc, train[, numeric_cols])
test_scaled <- predict(preproc, test[, numeric_cols])

# PCA Model
pca_model <- prcomp(train_scaled, center = FALSE, scale. = FALSE)
train_pca <- as.data.frame(predict(pca_model, train_scaled))
test_pca  <- as.data.frame(predict(pca_model, test_scaled))

# test_scaled <- predict(preproc, test[, numeric_cols])

# pred_all <- knn(train = train_scaled,
#                 test  = test_scaled,
#                 cl    = train$Survived,
#                 k     = 150)   

# cm <- confusionMatrix(pred_all, test$Survived)
# cm


# plot_boundary_plotly <- function(k) {
#   grid_pred <- knn(train = train_scaled, test = grid_expand,
#                    cl = train$Survived, k = k)
  
#   z_matrix <- matrix(as.numeric(as.character(grid_pred)),
#                      nrow = length(fare_seq),
#                      ncol = length(age_seq))
  
#   test_df <- data.frame(test_scaled, Survived = test$Survived)
  
#   plot_ly() %>%
#     # Filled contour = sharp decision regions
#     add_contour(x = age_seq, y = fare_seq, z = z_matrix,
#                 colorscale = list(c(0, "#F4CCCC"), c(1, "#C6D9F0")),
#                 contours = list(
#                   start = 0, end = 1, size = 0.5,
#                   coloring = "fill",
#                   showlines = TRUE
#                 ),
#                 line = list(color = "black", width = 1.5),
#                 showscale = FALSE,
#                 opacity = 0.6) %>%
#     # Did not survive
#     add_markers(data = test_df[test_df$Survived == 0, ],
#                 x = ~Age, y = ~Fare,
#                 marker = list(color = "#CC0000", size = 6,
#                               line = list(color = "white", width = 0.5)),
#                 name = "0 - Did not survive") %>%
#     # Survived
#     add_markers(data = test_df[test_df$Survived == 1, ],
#                 x = ~Age, y = ~Fare,
#                 marker = list(color = "#1A6FBF", size = 6,
#                               line = list(color = "white", width = 0.5)),
#                 name = "1 - Survived") %>%
#     layout(title = paste0("2-Class Classification (k = ", k, ")"),
#            xaxis = list(title = "Age (scaled)"),
#            yaxis = list(title = "Fare (scaled)"),
#            legend = list(title = list(text = "Actual")))
# }

# plot_boundary_plotly(150)

# addmargins(cM$table)
# cm$overall["Accuracy"]
# Compute mean & sd from train for scaling
# Apply to train and test

# Evaluate
# cM=confusionMatrix(knn_pred, test$Survived)
# addmargins(cM$table)
# cM$byClass
# cM$overall["Accuracy"]    
