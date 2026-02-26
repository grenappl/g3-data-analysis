library("ggplot2")
set.seed(123)
eda_data <- data.frame(
  StudentID = 1:120,
  StudyHours = runif(120, 1, 12),
  Attendance = runif(120, 60, 100),
  ScienceScore = round(
    40 + 4 * runif(120, 1, 12) + rnorm(120, 0, 8)
  ),
  Month = rep(month.abb[1:6], each = 20)
)
p <- ggplot(eda_data, aes(x = Month, y = ScienceScore, fill = Month)) +
  geom_boxplot() +
  labs(title = "Box and Whisker Plot")

p
