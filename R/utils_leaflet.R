
leaf_basemap <- function(
    bbox,
    baseGroups = c("Light", "OSM", "OSM HOT"),
    overlayGroups = c("Boundaries", "Taux d'attaque"),
    miniMap = TRUE
) {
  lf <- leaflet::leaflet() %>%
    leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
    leaflet::addMapPane(name = "boundaries", zIndex = 300) %>%
    leaflet::addMapPane(name = "choropleth", zIndex = 310) %>%
    leaflet::addMapPane(name = "circles", zIndex = 410) %>%
    leaflet::addMapPane(name = "region_highlight", zIndex = 420) %>%
    leaflet::addMapPane(name = "place_labels", zIndex = 320) %>%
    leaflet::addProviderTiles("CartoDB.PositronNoLabels", group = "Light") %>%
    leaflet::addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      group = "Light",
      options = leaflet::leafletOptions(pane = "place_labels")
    ) %>%
    leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>%
    leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
    leaflet::addScaleBar(
      position = "bottomright",
      options = leaflet::scaleBarOptions(imperial = FALSE)
    ) %>%
    leaflet.extras::addResetMapButton() %>%
    leaflet::addLayersControl(
      baseGroups = baseGroups,
      overlayGroups = overlayGroups,
      position = "topleft"
    )

  if (miniMap) {
    lf <- lf %>% leaflet::addMiniMap(toggleDisplay = TRUE, position = "bottomleft")
  }

  return(lf)
}
