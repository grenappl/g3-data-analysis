library(titanic)
library(class)
library(caret)

# color palette
COL_DARK  <- "#2A2521"
COL_AMBER <- "#AC6C35"
COL_CREAM <- "#DBCEBF"
COL_NAVY  <- "#344C65"
COL_GOLD  <- "#E9BA24"

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

# PCA Model
### had to comment this cuz error
# pca_model <- prcomp(train_scaled, center = FALSE, scale. = FALSE)
# train_pca <- as.data.frame(predict(pca_model, train_scaled))
# test_pca  <- as.data.frame(predict(pca_model, test_scaled))

team <- list(
  list(name = "Joenell de Pedro",  role = "Lead Data Scientist", bio = "Specialises in classification models and predictive analytics. Leads the KNN research pipeline.", color = COL_NAVY),
  list(name = "Ian Miguel Florentino",   role = "ML Engineer",         bio = "Builds and deploys scalable model pipelines. Maintains training infrastructure.", color = COL_AMBER),
  list(name = "Drixyl Nacu",    role = "Data Analyst",        bio = "Handles EDA, data cleaning, and feature engineering on structured historical datasets.", color = "#5a7a50"),
  list(name = "Ivan Paul Ruelan", role = "Visualisation Dev",   bio = "Designs dashboard interfaces and interactive data visualisations for stakeholder reporting.", color = COL_DARK),
  list(name = "Anakin Seno",   role = "Research Intern",     bio = "Supports model evaluation and documentation; contributes to hyperparameter tuning experiments.", color = "#7a5a40")
)