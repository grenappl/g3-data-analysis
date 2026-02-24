library(leaflet)

# Converted standalone leaflet code into a function for Shiny
geographical_plot <- function() {
  
  locations <- data.frame(
    name = c(
      "University of San Carlos - Talamban Campus",
      "SM City Cebu",
      "Cebu IT Park"
    ),
    lat = c(10.3540, 10.3113, 10.3316),
    lng = c(123.9115, 123.9180, 123.9066),
    importance = c(3, 2, 4)
  )
  
  pal <- colorNumeric(
    palette = c("#00C6FF", "#7F00FF"),
    domain = locations$importance
  )
  
  leaflet(locations) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(
      lng1 = min(locations$lng),
      lat1 = min(locations$lat),
      lng2 = max(locations$lng),
      lat2 = max(locations$lat)
    ) %>%
    addCircleMarkers(
      lng = ~lng,
      lat = ~lat,
      radius = ~importance * 5,
      color = ~pal(importance),
      stroke = TRUE,
      weight = 2,
      fillOpacity = 0.9,
      popup = ~paste0(
        "<div style='font-family: Arial;'>",
        "<h4>", name, "</h4>",
        "<p>Strategic Location in Cebu City</p>",
        "</div>"
      ),
      clusterOptions = markerClusterOptions()
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = ~importance,
      title = "Location Importance",
      opacity = 1
    ) %>%
    addMiniMap(toggleDisplay = TRUE)
}
