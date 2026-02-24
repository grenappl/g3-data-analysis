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
    type="bar"
  ) %>% layout(title = "Monthly Average of Science Scores")
}
box_plot <- plot_ly(
  eda_data,
  x = ~Month,
  y = ~ScienceScore,
  type = "box"
)

box_plot <- function() {
  p <- ggplot(eda_data,
              aes(x = Month, y = ScienceScore, fill = Month)) +
    geom_boxplot() +
    labs(title = "Box and Whisker Plot")
  
  ggplotly(p)
}

