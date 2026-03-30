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
# head(titanic)
set.seed(33)
# features <- scale(titanic[, c("Pclass", "Sex", "Age", "Fare")])
# labels <- titanic$Survived
train_index = createDataPartition(titanic$Survived, p =0.7, list=FALSE)
train <- titanic[train_index, ]
test <- titanic[-train_index, ]

# Scale only numeric features (exclude Survived)
numeric_cols <- c("Pclass", "Sex", "Age", "Fare")

# Compute mean & sd from train for scaling
train_mean <- apply(train[, numeric_cols], 2, mean)
train_sd   <- apply(train[, numeric_cols], 2, sd)

# Scale train features
train_scaled <- train
train_scaled[, numeric_cols] <- sweep(train[, numeric_cols], 2, train_mean, "-")
train_scaled[, numeric_cols] <- sweep(train_scaled[, numeric_cols], 2, train_sd, "/")

# Scale test features using train mean & sd
test_scaled <- test
test_scaled[, numeric_cols] <- sweep(test[, numeric_cols], 2, train_mean, "-")
test_scaled[, numeric_cols] <- sweep(test_scaled[, numeric_cols], 2, train_sd, "/")

head(train_scaled)

knn_pred <- knn(train_scaled[, numeric_cols], 
                test_scaled[, numeric_cols], 
                train_scaled$Survived, k = 3)

# Evaluate
cM=confusionMatrix(knn_pred, test$Survived)
addmargins(cM$table)
cM$byClass
mean(knn_pred == test_scaled$Survived)  # accuracy
