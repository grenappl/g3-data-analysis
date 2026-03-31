library(titanic)
library(class)
library(caret)
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

# Scale only numeric features (exclude Survived)
numeric_cols <- c("Pclass", "Sex", "Age", "Fare")
preproc <- preProcess(train[, numeric_cols], method = c("center", "scale"))
train_scaled <- predict(preproc, train[, numeric_cols])
# test_scaled_full <- predict(preproc, test[, numeric_cols])

# pred_all <- knn(train = train_scaled,
#                 test  = test_scaled_full,
#                 cl    = train$Survived,
#                 k     = 30)   

# cm <- confusionMatrix(pred_all, test$Survived)
# addmargins(cM$table)
# cm$overall["Accuracy"]
# Compute mean & sd from train for scaling
# Apply to train and test

# Evaluate
# cM=confusionMatrix(knn_pred, test$Survived)
# addmargins(cM$table)
# cM$byClass
# cM$overall["Accuracy"]    
