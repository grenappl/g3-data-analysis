library(titanic)
library(class)
library(caret)

data(titanic_train)
titanic <- titanic_train[, c("Survived", "Pclass", "Sex", "Age", "Fare")]
titanic <- na.omit(titanic)
titanic$Survived <- factor(titanic$Survived)
titanic$Sex <- ifelse(titanic$Sex == "male", 0, 1)

set.seed(33)
train_index <- createDataPartition(titanic$Survived, p = 0.7, list = FALSE)
train <- titanic[train_index, ]
test  <- titanic[-train_index, ]

numeric_cols <- c("Pclass", "Sex", "Age", "Fare")
preproc     <- preProcess(train[, numeric_cols], method = c("center", "scale"))
train_scaled <- predict(preproc, train[, numeric_cols])
test_scaled  <- predict(preproc, test[, numeric_cols])

# Test a range of k values
k_values  <- c(1, 3, 5, 10, 20, 30, 50, 75, 100, 150)
accuracies <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  pred <- knn(train = train_scaled,
              test  = test_scaled,
              cl    = train$Survived,
              k     = k_values[i])
  cm <- confusionMatrix(pred, test$Survived)
  print(addmargins(cm$table))
  accuracies[i] <- cm$overall["Accuracy"]
}

# Display results
results <- data.frame(k = k_values, Accuracy = round(accuracies, 4))
print(results)

# Plot
plot(k_values, accuracies,
     type = "b",
     pch  = 19,
     col  = "steelblue",
     xlab = "k (Number of Neighbors)",
     ylab = "Accuracy",
     main = "KNN: Accuracy vs k (Titanic Dataset)",
     ylim = c(0.6, 1.0))
abline(h = max(accuracies), col = "red", lty = 2)
text(k_values[which.max(accuracies)], max(accuracies),
     labels = paste0("Best k=", k_values[which.max(accuracies)]),
     pos = 4, col = "red", cex = 0.8)