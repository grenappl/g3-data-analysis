library(leaflet)

manila_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(
    lng = 120.9842,
    lat = 14.5995,
    zoom = 13
  ) %>%
  addMarkers(
    lng = 120.9842,
    lat = 14.5995,
    popup = "<b>Manila City</b><br>Capital of the Philippines"
  )

manila_map

library(htmlwidgets)

saveWidget(
  manila_map,
  "CG2CG3.html",
  selfcontained = TRUE
)
