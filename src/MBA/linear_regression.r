# 1. Clear environment (optional but recommended to prevent ghost variables)
rm(list = ls())

# 2. Set seed and define n
set.seed(33)
n <- 100

# 3. Generate the dataframe
data <- data.frame(
  StudyHours = round(runif(n, 2, 20), 1),
  AttendanceRate = round(runif(n, 60, 100), 1),
  Gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  SchoolType = factor(sample(c("Public", "Private"), n, replace = TRUE))
)

# 4. Calculate ScienceScore
data$ScienceScore <- round(
  40 +
    2.5 * data$StudyHours +
    0.3 * data$AttendanceRate +
    ifelse(data$Gender == "Female", 3, 0) +
    ifelse(data$SchoolType == "Private", 5, 0) +
    rnorm(n, 0, 5), 1
)

head(data)
model <- lm(ScienceScore ~ ., data = data)

# View the coefficients (Intercept and Slope) to form your equation
coef(model)

summary(model)