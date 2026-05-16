library("rpart")
library("caret")
library("rpart.plot")

titanic <- titanic_train[,
    c("Survived", "Pclass", "Sex", "Age", "Fare")]
titanic <- na.omit(titanic)

titanic$Survived <- factor(titanic$Survived)
set.seed(33)
train_index = createDataPartition(titanic$Survived, p =0.7, list=FALSE)
train <- titanic[train_index, ]
test <- titanic[-train_index, ]

tree_model <- rpart(Survived ~ Pclass + Sex + Age + Fare, data=train,
                    method="class", minsplit=10) 
rpart.plot(tree_model, type=2, 
            fallen.leaves = TRUE,
            main = "Titanic Survival Classification Tree" )
printcp(tree_model)
plotcp(tree_model)

best_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
print(best_cp)

pruned_tree <- prune(tree_model, cp = best_cp)
rpart.plot(pruned_tree, type = 4, extra = 104, main = "Pruned Tree")
pred_pruned <- predict(pruned_tree, test, type = "class")
confusionMatrix(pred_pruned, test$Survived)