library(leaflet)

geographical_plot <- function() {
  
  # Cebu locations with exact coordinates
  locations <- data.frame(
    name = c(
      "University of San Carlos - Talamban Campus",
      "University of Cebu - Banilad Campus",
      "SM City Cebu",
      "Ayala Center Cebu",
      "Cebu IT Park",
      "Tops Lookout",
      "Fort San Pedro"
    ),
    lat = c(
      10.3523649097122,
      10.338664319959733,
      10.311725200859122,
      10.31849031762046,
      10.329704468960365,
      10.37082516105934,
      10.292668113628277
    ),
    lng = c(
      123.91326756922604,
      123.91164258016562,
      123.9177520223094,
      123.9052616818272,
      123.90678952246283,
      123.87078556463982,
      123.9056574529915
    ),
    importance = c(5, 4, 5, 4, 4, 3, 3),
    icon_type = c(
      "university",
      "university",
      "shopping-cart",
      "shopping-bag",
      "laptop",
      "binoculars",
      "landmark"
    )
  )
  
  # Awesome icons for markers
  icons <- awesomeIcons(
    icon = locations$icon_type,
    iconColor = "white",
    library = "fa",
    markerColor = "purple"
  )
  
  # Color palette for importance
  pal <- colorNumeric(
    palette = c("#00C6FF", "#7F00FF"),
    domain = locations$importance
  )
  
  # Create leaflet map
  leaflet(locations) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% # light map
    fitBounds(
      lng1 = min(locations$lng) - 0.01,
      lat1 = min(locations$lat) - 0.01,
      lng2 = max(locations$lng) + 0.01,
      lat2 = max(locations$lat) + 0.01
    ) %>%
    addAwesomeMarkers(
      lng = ~lng,
      lat = ~lat,
      icon = icons,
      popup = ~paste0(
        "<div style='font-family: Arial; text-align:center; width:220px;'>",
        "<h4 style='color:#7F00FF;'>", name, "</h4>",
        "<p>Importance Level: <b>", importance, "</b></p>",
        "<a href='https://www.google.com/maps/search/?api=1&query=", lat, ",", lng, "' target='_blank'>View on Google Maps</a>",
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
    addMiniMap(
      toggleDisplay = TRUE,
      tiles = providers$Stamen.TonerLite
    )
}
