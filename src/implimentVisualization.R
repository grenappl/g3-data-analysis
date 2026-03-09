
library("plotly")
library("dplyr")
library("ggplot2")
set.seed(33)
eda_data <- data.frame(
  StudentID = 1:120,
  StudyHours = runif(120, 1, 12),
  Attendance = runif(120, 60, 100),
  ScienceScore = round(
    40 + 4 * runif(120, 1, 12) + rnorm(120, 0, 8)
  ),
  Month = rep(month.abb[1:6], each = 20)
)
scatter_plot <- function(x_var, y_var){
  plot_ly(
    eda_data,
    x = ~get(x_var),
    y = ~get(y_var),
    type = "scatter",
    mode = "markers",
    labels = list(
      x =  x_var,
      y =  y_var
    )
    
  ) %>% layout(title = paste(x_var, " vs ", y_var), 
              xaxis = list(title = x_var), 
              yaxis = list(title = y_var), 
              plot_bgcolor = "#e5ecf6")

}

bar_plot <- function(){
  avg_scores <- eda_data %>% group_by(Month) %>% summarize(ScienceScore = mean(ScienceScore))

  plot_ly(
    avg_scores,
    x = ~Month,
    y = ~ScienceScore,
    color = ~Month,
    type="bar"
  ) %>% layout(title = "Monthly Average of Science Scores")
}

plot_histogram <- function(x_var, bins){
  plot_ly(
    eda_data,
    x = ~.data[[x_var]],
    type = "histogram",
    nbinsx = bins,
    marker = list(
      color = "#422feb",
      line = list(
        color = "black",
        width = 1
      )
    )
  ) %>%
  layout(
    xaxis = list(title = x_var),
    yaxis = list(title = "Count"),
    height = 400
  )
}

box_plot <- function() {
  plot_ly(
    data = eda_data,
    x = ~Month,
    y = ~ScienceScore,
    color = ~Month,
    type = "box"
  )
}
