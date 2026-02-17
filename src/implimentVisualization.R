library("plotly")
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
scatter_plot <- plot_ly(
  eda_data,
  x = ~StudyHours,
  y = ~ScienceScore,
  type = "scatter",
  mode = "markers"
)
box_plot <- plot_ly(
  eda_data,
  x = ~Month,
  y = ~ScienceScore,
  type = "box"
)


