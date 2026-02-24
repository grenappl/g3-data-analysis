box_plot <- function() {
  plot_ly(
    data = eda_data,
    x = ~Month,
    y = ~ScienceScore,
    color = ~Month,
    type = "box"
  )
}
