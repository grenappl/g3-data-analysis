library(titanic)
library(class)
library(caret)

# Load and prepare data
data(titanic_train)
titanic <- titanic_train[, c("Survived", "Pclass", "Sex", "Age", "Fare")]
titanic <- na.omit(titanic)
titanic$Survived <- factor(titanic$Survived)
titanic$Sex <- factor(titanic$Sex)

# Encode Sex as numeric (required for KNN)
titanic$Sex_num <- ifelse(titanic$Sex == "male", 1, 0)

# Select features for modeling
features <- c("Pclass", "Sex_num", "Age", "Fare")
X <- titanic[, features]
y <- titanic$Survived

# Normalize features
preproc <- preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preproc, X)

# Train/test split (80/20)
set.seed(42)
train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_scaled[train_idx, ]
X_test  <- X_scaled[-train_idx, ]
y_train <- y[train_idx]
y_test  <- y[-train_idx]

# --- Find Optimal K via Cross-Validation ---
# Now tracking multiple metrics across all K values
ctrl <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,          # needed for AUC/ROC
  summaryFunction = twoClassSummary,  # returns ROC, Sens, Spec
  savePredictions = "final"
)

# Rename factor levels to valid R names (required by twoClassSummary)
y_train_named <- factor(y_train, labels = c("Died", "Survived"))
y_test_named  <- factor(y_test,  labels = c("Died", "Survived"))

knn_cv <- train(
  x        = X_train,
  y        = y_train_named,
  method   = "knn",
  trControl = ctrl,
  tuneGrid  = data.frame(k = 1:150),
  metric    = "ROC"               # optimize on AUC instead of Accuracy
)

print(knn_cv)
best_k <- knn_cv$bestTune$k
cat("Best K:", best_k, "\n")

# --- Plot 1: Accuracy by K ---
plot(knn_cv, metric = "ROC",  main = "KNN ROC (AUC) by K (10-Fold CV)")

# --- Plot 2: All CV Metrics Across K values ---
cv_results <- knn_cv$results  # ROC, Sens, Spec for every K

par(mfrow = c(1, 1))
plot(cv_results$k, cv_results$ROC,
     type = "l", col = "blue", lwd = 2,
     ylim = c(0.5, 1.0),
     xlab = "K (Neighbors)",
     ylab = "Metric Value (CV)",
     main = "KNN CV Metrics Across K Values")
lines(cv_results$k, cv_results$Sens, col = "green",  lwd = 2)
lines(cv_results$k, cv_results$Spec, col = "red",    lwd = 2)
abline(v = best_k, lty = 2, col = "gray40")
legend("bottomright",
       legend = c("ROC (AUC)", "Sensitivity", "Specificity"),
       col    = c("blue", "green", "red"),
       lwd    = 2)

# --- Final Prediction with Best K ---
knn_pred <- knn(
  train = X_train,
  test  = X_test,
  cl    = y_train_named,
  k     = best_k
)

# --- Evaluation ---
cm <- confusionMatrix(knn_pred, y_test_named, positive = "Survived")
print(cm)

# --- Key Metrics ---
acc  <- round(cm$overall["Accuracy"],    4)
sens <- round(cm$byClass["Sensitivity"], 4)
spec <- round(cm$byClass["Specificity"], 4)
prec <- round(cm$byClass["Precision"],   4)
f1   <- round(cm$byClass["F1"],          4)

cat("\n--- Key Metrics ---\n")
cat("Accuracy:            ", acc,  "\n")
cat("Sensitivity (Recall):", sens, "\n")
cat("Specificity:         ", spec, "\n")
cat("Precision:           ", prec, "\n")
cat("F1 Score:            ", f1,   "\n")

# --- Imbalance Bias Check ---
cat("\n--- Imbalance Bias Check ---\n")
diff <- abs(sens - spec)
if (diff > 0.10) {
  cat("⚠ WARNING: Sensitivity and Specificity differ by", round(diff, 4),
      "— model may be biased toward the majority class.\n")
  cat("  Consider applying SMOTE or oversampling.\n")
} else {
  cat("✔ Sensitivity and Specificity are reasonably balanced (diff =",
      round(diff, 4), ") — no major bias detected.\n")
}

# --- Plot 3: Metrics Bar Chart (Test Set) ---
metrics      <- c(acc, sens, spec, prec, f1)
metric_names <- c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1")
bar_colors   <- c("steelblue", "green3", "tomato", "orchid", "orange")

barplot(metrics,
        names.arg = metric_names,
        col       = bar_colors,
        ylim      = c(0, 1),
        main      = "KNN Test Set Performance Metrics",
        ylab      = "Score",
        border    = NA)
abline(h = 0.5, lty = 2, col = "gray")
text(x      = seq(0.7, by = 1.2, length.out = 5),
     y      = metrics + 0.03,
     labels = metrics,
     cex    = 0.9,
     font   = 2)